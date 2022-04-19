//! Code to convert AST into untyped IR

const std = @import("std");
const bexpr = @import("bexpr");
const ir = @import("ir.zig");

pub const Generator = struct {
    arena: std.heap.ArenaAllocator,
    exports: std.StringHashMapUnmanaged(ir.Export) = .{},
    globals: std.StringHashMapUnmanaged(*ir.Global) = .{},

    pub fn init(base_allocator: std.mem.Allocator) Generator {
        return Generator{ .arena = std.heap.ArenaAllocator.init(base_allocator) };
    }
    pub fn deinit(self: *Generator) void {
        self.arena.deinit();
    }

    pub const Error = error{
        InvalidCode,
        OutOfMemory,
    };

    pub fn module(self: *Generator, ast: []const bexpr.Value) Error!ir.Module {
        for (ast) |tl| {
            try self.toplevel(tl);
        }

        const mod = ir.Module{
            .arena = self.arena,
            .exports = self.exports,
        };
        // Reset generator
        self.* = init(self.arena.child_allocator);

        return mod;
    }

    fn toplevel(self: *Generator, tl: bexpr.Value) Error!void {
        if (tl != .call or tl.call.len < 1) {
            return error.InvalidCode;
        }

        var args = tl.call;
        var public = false;
        if (args[0] == .symbol and std.mem.eql(u8, "pub", args[0].symbol)) {
            public = true;
            args = args[1..];
        }

        if (args.len < 1 or args[0] != .symbol) {
            return error.InvalidCode;
        }
        const func = toplevel_table.get(args[0].symbol) orelse {
            return error.InvalidCode;
        };
        // TODO: order-independence
        try func(self, public, args[1..]);
    }

    const toplevel_table = std.ComptimeStringMap(fn (*Generator, bool, []const bexpr.Value) Error!void, .{
        .{ "use", tlUse },
        .{ "fn", tlFn },
    });

    fn tlUse(self: *Generator, public: bool, args: []const bexpr.Value) Error!void {
        if (args.len != 2) {
            return error.InvalidCode;
        }

        if (args[0] != .symbol) {
            return error.InvalidCode;
        }
        const mod = args[0].symbol;

        if (args[1] != .list) {
            return error.InvalidCode;
        }
        for (args[1].list) |val| {
            if (val != .symbol) {
                return error.InvalidCode;
            }
            _ = try self.global(val.symbol, .{ .import = .{
                .mod = mod,
                .sym = val.symbol,
            } });
        }

        if (public) {
            return error.InvalidCode; // TODO
        }
    }

    fn tlFn(self: *Generator, public: bool, args: []const bexpr.Value) Error!void {
        if (args.len < 2) {
            return error.InvalidCode;
        }

        if (args[0] != .symbol) {
            return error.InvalidCode;
        }
        const name = args[0].symbol;

        if (args[args.len - 1] != .block) {
            return error.InvalidCode;
        }
        const body = args[args.len - 1].block;

        var gen = FunctionGenerator{ .mod = self };
        const func = try gen.function(name, args[1 .. args.len - 1], body);

        if (public) {
            try self.exports.put(
                self.arena.allocator(),
                func.name,
                .{ .func = func },
            );
        }
    }

    fn global(self: *Generator, name: []const u8, glo: ir.Global) Error!*ir.Global {
        const gop = try self.globals.getOrPut(self.arena.allocator(), name);
        if (gop.found_existing) {
            return error.InvalidCode;
        }
        const glo_ptr = try self.arena.allocator().create(ir.Global);
        glo_ptr.* = glo;
        gop.value_ptr.* = glo_ptr;
        return glo_ptr;
    }
};

const FunctionGenerator = struct {
    mod: *Generator,
    temp_i: u32 = 0,
    locals: std.StringArrayHashMapUnmanaged(ir.Temporary) = .{},

    block: *ir.Block = undefined,
    insns: std.ArrayListUnmanaged(ir.Instruction) = .{},

    fn deinit(self: *FunctionGenerator) void {
        self.locals.deinit(self.mod.arena.allocator());
    }

    // TODO: variadic
    fn function(
        self: *FunctionGenerator,
        name: []const u8,
        params: []const bexpr.Value,
        body: []const bexpr.Value,
    ) !*ir.Function {
        const allocator = self.mod.arena.allocator();

        for (params) |param| {
            if (param != .symbol) {
                return error.InvalidCode;
            }
            try self.locals.put(allocator, param.symbol, self.newTemp());
        }

        self.block = try allocator.create(ir.Block);

        const glo = try self.mod.global(name, .{ .func = .{
            .name = name,
            .arity = @intCast(u32, params.len),
            .entry = self.block,
        } });

        for (body) |stmt| {
            try self.statement(stmt);
        }
        self.block.insns = self.insns.toOwnedSlice(allocator);
        self.block.term = .{ .ret = null };

        return &glo.func;
    }

    fn statement(self: *FunctionGenerator, stmt: bexpr.Value) !void {
        if (stmt != .call or stmt.call.len < 1) {
            return error.InvalidCode;
        }

        if (special(stmt_special, stmt.call[0])) |fun| {
            try fun(self, stmt.call[1..]);
        } else {
            _ = try self.expression(stmt);
        }
    }

    fn expression(self: *FunctionGenerator, expr: bexpr.Value) Generator.Error!ir.Temporary {
        switch (expr) {
            .call => |call| {
                const args = try self.mod.arena.allocator().alloc(ir.Temporary, call.len);
                for (call) |arg, i| {
                    args[i] = try self.expression(arg);
                }
                return self.emit(.call, args);
            },

            .number => |num| {
                if (std.fmt.parseInt(i64, num, 0)) |int| {
                    return self.emit(.int, int);
                } else |_| if (std.fmt.parseFloat(f64, num)) |flt| {
                    return self.emit(.float, flt);
                } else |_| {
                    return error.InvalidCode;
                }
            },

            .string => |str| return self.emit(.str, str),
            .symbol => return self.load(try self.lvalue(expr)),

            else => return error.InvalidCode, // TODO
        }
    }
    fn lvalue(self: *FunctionGenerator, expr: bexpr.Value) !LValue {
        switch (expr) {
            .symbol => |sym| {
                if (self.locals.contains(sym)) {
                    return LValue{ .local = sym };
                } else {
                    return LValue{ .global = sym };
                }
            },
            else => return error.InvalidCode,
        }
    }

    fn special(table: anytype, val: bexpr.Value) @typeInfo(@TypeOf(table.get)).Fn.return_type.? {
        return switch (val) {
            .symbol => |sym| table.get(sym),
            .operator => |op| table.get(op),
            else => null,
        };
    }
    const stmt_special = std.ComptimeStringMap(StmtFn, .{
        .{ ":=", stmtDefine },
        .{ "=", stmtAssign(null) },
        .{ "+=", stmtAssign(.add) },
        // .{ "if", stmtIf },
    });
    const StmtFn = fn (*FunctionGenerator, []const bexpr.Value) Generator.Error!void;
    const expr_special = std.ComptimeStringMap(ExprFn, .{
        .{ "+", exprOp(.add) },
    });
    const ExprFn = fn (*FunctionGenerator, []const bexpr.Value) Generator.Error!ir.Temporary;

    fn stmtDefine(self: *FunctionGenerator, args: []const bexpr.Value) !void {
        if (args.len != 2) {
            return error.InvalidCode;
        }
        if (args[0] != .symbol) {
            return error.InvalidCode;
        }

        const v = try self.expression(args[1]);
        try self.store(.{ .local = args[0].symbol }, v);
    }

    fn stmtAssign(comptime maybe_op: ?std.meta.Tag(ir.Instruction.Op)) StmtFn {
        return struct {
            fn assign(self: *FunctionGenerator, args: []const bexpr.Value) !void {
                if (args.len != 2) {
                    return error.InvalidCode;
                }

                const l = try self.lvalue(args[0]);
                const r = try self.expression(args[1]);

                if (maybe_op) |op| {
                    const t = try self.emit(op, .{ try self.load(l), r });
                    try self.store(l, t);
                } else {
                    try self.store(l, r);
                }
            }
        }.assign;
    }

    fn exprOp(comptime op: std.meta.Tag(ir.Instruction.Op)) ExprFn {
        return struct {
            fn op(self: *FunctionGenerator, args: []const bexpr.Value) !ir.Temporary {
                if (args.len < 1) {
                    return error.InvalidCode;
                } else if (args.len == 1) {
                    // Apply operator with "identity" value
                    const id = switch (op) {
                        .add => 0,
                    };
                    const a = try self.emit(.int, id);
                    const b = try self.expression(args[0]);
                    return self.emit(op, .{ a, b });
                } else {
                    // Apply operator from left to right
                    var a = try self.expression(args[0]);
                    for (args[1..]) |arg| {
                        const b = try self.expression(arg);
                        a = try self.emit(op, .{ a, b });
                    }
                    return a;
                }
            }
        }.op;
    }

    fn load(self: *FunctionGenerator, lv: LValue) !ir.Temporary {
        switch (lv) {
            .local => |name| {
                return self.locals.get(name) orelse error.InvalidCode;
            },
            .global => |name| {
                const glo = self.mod.globals.get(name) orelse return error.InvalidCode;
                return self.emit(.global, glo);
            },
        }
    }
    fn store(self: *FunctionGenerator, lv: LValue, v: ir.Temporary) !void {
        switch (lv) {
            .local => |name| {
                try self.locals.put(self.mod.arena.allocator(), name, v);
            },
            .global => return error.InvalidCode,
        }
    }
    const LValue = union(enum) {
        local: []const u8,
        global: []const u8,
    };

    fn emit(
        self: *FunctionGenerator,
        comptime op: std.meta.Tag(ir.Instruction.Op),
        params: std.meta.TagPayload(ir.Instruction.Op, op),
    ) !ir.Temporary {
        const result = self.newTemp();
        try self.insns.append(self.mod.arena.allocator(), .{
            .result = result,
            .op = @unionInit(ir.Instruction.Op, @tagName(op), params),
        });
        return result;
    }

    fn newTemp(self: *FunctionGenerator) ir.Temporary {
        const t = @intToEnum(ir.Temporary, self.temp_i);
        self.temp_i += 1;
        return t;
    }
};
