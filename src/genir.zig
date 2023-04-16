//! Code to convert AST into untyped IR

const std = @import("std");
const bexpr = @import("bexpr");
const ir = @import("ir.zig");

pub const Generator = struct {
    arena: std.heap.ArenaAllocator,
    deps: std.ArrayListUnmanaged([]const u8) = .{},
    exports: std.StringHashMapUnmanaged(ir.GlobalId) = .{},
    globals: std.ArrayListUnmanaged(ir.Global) = .{},
    global_names: std.StringHashMapUnmanaged(ir.GlobalId) = .{},
    strings: std.StringHashMapUnmanaged(void) = .{},

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
            .deps = try self.deps.toOwnedSlice(self.arena.allocator()),
            .globals = try self.globals.toOwnedSlice(self.arena.allocator()),
            .exports = self.exports,
        };
        // Reset generator
        self.* = init(self.arena.child_allocator);

        return mod;
    }

    fn toplevel(self: *Generator, tl: bexpr.Value) Error!void {
        if (tl == .list) return; // TODO: Use lists as pseudo-comments for now
        if (tl != .call or tl.call.len < 1) {
            return error.InvalidCode;
        }

        var args = tl.call;
        var public = false;
        if (args[0] == .symbol and std.mem.eql(u8, "pub", args[0].symbol)) {
            public = true;
            if (args.len == 2 and args[1] == .call) {
                args = args[1].call;
            } else {
                args = args[1..];
            }
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

    const toplevel_table = std.ComptimeStringMap(*const fn (*Generator, bool, []const bexpr.Value) Error!void, .{
        .{ "use", tlUse },
        .{ "fn", tlFn },
        .{ "extern", tlExtern },
        .{ ":=", tlDefine },
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
            _ = try self.global(val.symbol, public, .{ .import = .{
                .mod = try self.intern(mod),
                .sym = try self.intern(val.symbol),
            } });
        }

        try self.deps.append(self.arena.allocator(), try self.intern(mod));
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
        try gen.function(name, public, args[1 .. args.len - 1], body);
    }

    fn tlExtern(self: *Generator, public: bool, extern_args: []const bexpr.Value) Error!void {
        if (extern_args.len < 2) {
            return error.InvalidCode;
        }

        if (extern_args[0] != .string) {
            return error.InvalidCode;
        }
        const mod = extern_args[0].string;

        if (extern_args[1] != .symbol) {
            return error.InvalidCode;
        }
        const kw = extern_args[1].symbol;

        const args = extern_args[2..];
        if (std.mem.eql(u8, kw, "fn")) {
            if (args.len < 1 or args[0] != .symbol) {
                return error.InvalidCode;
            }
            const name = args[0].symbol;

            var params = std.ArrayList(ir.Type).init(self.arena.allocator());
            errdefer params.deinit();
            var ret: ?ir.Type = null;

            for (args[1..]) |param| {
                if (ret != null) return error.InvalidCode;

                if (param == .call and param.call.len > 0 and param.call[0] == .symbol) {
                    if (std.mem.eql(u8, param.call[0].symbol, ":")) {
                        if (param.call.len != 3) { // (name : type)
                            return error.InvalidCode;
                        }
                        const ty = try self.parseType(param.call[2]);
                        try params.append(ty);
                    } else if (std.mem.eql(u8, param.call[0].symbol, "->")) {
                        if (param.call.len != 2) { // (-> type)
                            return error.InvalidCode;
                        }
                        ret = try self.parseType(param.call[1]);
                    } else {
                        const ty = try self.parseType(param);
                        try params.append(ty);
                    }
                } else {
                    const ty = try self.parseType(param);
                    try params.append(ty);
                }
            }

            _ = try self.global(name, public, .{ .extern_func = .{
                .name = try self.intern(name),
                .mod = try self.intern(mod),
                .params = try params.toOwnedSlice(),
                .ret = ret,
            } });
        } else {
            return error.InvalidCode;
        }
    }

    fn parseType(self: *Generator, val: bexpr.Value) !ir.Type {
        switch (val) {
            .symbol => |sym| {
                if (std.meta.stringToEnum(ir.Type.Base, sym)) |base_ty| {
                    return ir.Type{ .base = base_ty };
                } else {
                    return ir.Type{ .name = try self.intern(sym) };
                }
            },
            else => return error.InvalidCode,
        }
    }

    fn tlDefine(self: *Generator, public: bool, args: []const bexpr.Value) Error!void {
        if (args.len != 2) {
            return error.InvalidCode;
        }
        if (args[0] != .symbol) {
            return error.InvalidCode;
        }
        const name = args[0].symbol;

        switch (args[1]) {
            .number => |num| {
                _ = try self.global(name, public, .{
                    .constant = try parseNum(num),
                });
            },

            .string => |str| {
                _ = try self.global(name, public, .{
                    .constant = .{ .string = try self.intern(str) },
                });
            },
            .symbol => |sym| {
                const glo = self.global_names.get(sym) orelse return error.InvalidCode;
                try self.nameGlobal(name, public, glo);
            },

            else => return error.InvalidCode,
        }
    }
    fn parseNum(num: []const u8) !ir.Constant {
        if (std.fmt.parseInt(i65, num, 0)) |int| {
            return ir.Constant{ .int = int };
        } else |_| if (std.fmt.parseFloat(f64, num)) |flt| {
            return ir.Constant{ .float = flt };
        } else |_| {
            return error.InvalidCode;
        }
    }

    fn global(self: *Generator, name: []const u8, public: bool, glo: ir.Global) Error!ir.GlobalId {
        const id = @intToEnum(ir.GlobalId, self.globals.items.len);
        try self.globals.append(self.arena.allocator(), glo);
        try self.nameGlobal(name, public, id);
        return id;
    }

    fn nameGlobal(self: *Generator, name: []const u8, public: bool, glo: ir.GlobalId) Error!void {
        const allocator = self.arena.allocator();

        const gop = try self.global_names.getOrPut(allocator, name);
        if (gop.found_existing) {
            return error.InvalidCode;
        }

        gop.value_ptr.* = glo;

        if (public) {
            try self.exports.putNoClobber(allocator, try self.intern(name), glo);
        }
    }

    fn intern(self: *Generator, str: []const u8) ![]const u8 {
        const gop = try self.strings.getOrPut(self.arena.allocator(), str);
        if (!gop.found_existing) {
            gop.key_ptr.* = try self.arena.allocator().dupe(u8, str);
        }
        return gop.key_ptr.*;
    }
};

const FunctionGenerator = struct {
    mod: *Generator,
    temp_i: u32 = 0,
    locals: std.StringArrayHashMapUnmanaged(ir.Temporary) = .{},

    blocks: std.ArrayListUnmanaged(ir.Block) = .{},
    insns: std.ArrayListUnmanaged(ir.Instruction) = .{},

    fn deinit(self: *FunctionGenerator) void {
        self.locals.deinit(self.mod.arena.allocator());
    }

    // TODO: variadic
    fn function(
        self: *FunctionGenerator,
        name: []const u8,
        public: bool,
        params: []const bexpr.Value,
        body: []const bexpr.Value,
    ) !void {
        const allocator = self.mod.arena.allocator();

        for (params) |param| {
            if (param != .symbol) {
                return error.InvalidCode;
            }
            try self.locals.put(allocator, param.symbol, self.newTemp());
        }

        const id = try self.mod.global(name, public, undefined);

        for (body) |stmt| {
            try self.statement(stmt);
        }
        try self.block(.{ .ret = null });

        self.mod.globals.items[@enumToInt(id)] = .{ .func = .{
            .name = try self.mod.intern(name),
            .arity = @intCast(u32, params.len),
            .ntemp = self.temp_i,
            .blocks = try self.blocks.toOwnedSlice(allocator),
        } };
    }

    fn block(self: *FunctionGenerator, term: ir.Block.Terminal) !void {
        try self.blocks.append(self.mod.arena.allocator(), .{
            .insns = try self.insns.toOwnedSlice(self.mod.arena.allocator()),
            .term = term,
        });
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
                if (special(expr_special, call[0])) |fun| {
                    return fun(self, call[1..]);
                }

                const args = try self.mod.arena.allocator().alloc(ir.Temporary, call.len);
                for (call, 0..) |arg, i| {
                    args[i] = try self.expression(arg);
                }
                return self.emit(.call, args);
            },

            .number => |num| {
                if (std.fmt.parseInt(i64, num, 0)) |i| {
                    return self.emit(.literal, .{ .int = i });
                } else |_| if (std.fmt.parseFloat(f64, num)) |f| {
                    return self.emit(.literal, .{ .float = f });
                } else |_| {
                    return error.InvalidCode;
                }
            },

            .string => |str| return self.emit(.literal, .{
                .string = try self.mod.intern(str),
            }),
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
        if (val != .symbol) return null;
        return table.get(val.symbol);
    }
    const StmtFn = *const fn (*FunctionGenerator, []const bexpr.Value) Generator.Error!void;
    const stmt_special = std.ComptimeStringMap(StmtFn, .{
        .{ ":=", stmtDefine },
        .{ "=", stmtAssign(null) },
        .{ "+=", stmtAssign(.add) },
        .{ "-=", stmtAssign(.sub) },
        .{ "*=", stmtAssign(.mul) },
        .{ "/=", stmtAssign(.div) },
        // .{ "if", stmtIf },
    });
    const ExprFn = *const fn (*FunctionGenerator, []const bexpr.Value) Generator.Error!ir.Temporary;
    const expr_special = std.ComptimeStringMap(ExprFn, .{
        .{ "+", exprOp(.add) },
        .{ "-", exprOp(.sub) },
        .{ "*", exprOp(.mul) },
        .{ "/", exprOp(.div) },
    });

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
            fn f(self: *FunctionGenerator, args: []const bexpr.Value) !ir.Temporary {
                if (args.len < 1) {
                    return error.InvalidCode;
                } else if (args.len == 1) {
                    // Apply operator with "identity" value
                    const id = switch (op) {
                        .add, .sub => 0,
                        .mul, .div => 1,
                        else => @compileError("unsupported operator"),
                    };
                    const a = try self.emit(.literal, .{ .int = id });
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
        }.f;
    }

    fn load(self: *FunctionGenerator, lv: LValue) !ir.Temporary {
        switch (lv) {
            .local => |name| {
                return self.locals.get(name) orelse error.InvalidCode;
            },
            .global => |name| {
                const glo = self.mod.global_names.get(name) orelse return error.InvalidCode;
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
