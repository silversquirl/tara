const std = @import("std");
const ir = @import("ir.zig");

pub const Analyzer = struct {
    allocator: std.mem.Allocator,
    modules: std.StringHashMapUnmanaged(ir.Module),
    instances: Instances = .{},

    pub fn init(allocator: std.mem.Allocator, modules: std.StringHashMapUnmanaged(ir.Module)) Analyzer {
        return .{ .allocator = allocator, .modules = modules };
    }

    pub fn analyze(self: *Analyzer) !Instances {
        // TODO: generalize to support arbitrary exported functions
        const main_mod = self.modules.getPtr("main") orelse return error.InvalidCode;
        const main_id = main_mod.exports.get("main") orelse return error.InvalidCode;
        const main_func = &main_mod.globals[@enumToInt(main_id)];

        var mod = ModuleAnalyzer{
            .root = self,
            .mod = main_mod,
        };
        _ = try mod.call(&main_func.func, &.{});

        return self.instances;
    }
};

const ModuleAnalyzer = struct {
    root: *Analyzer,
    mod: *const ir.Module,

    fn call(self: *ModuleAnalyzer, func: *const ir.Function, params: []const TypeSet) Error!TypeSet {
        const sig = Signature.init(func.*, params);
        const gop = try self.root.instances.getOrPut(self.root.allocator, sig);

        if (!gop.found_existing) {
            const types = try self.root.allocator.alloc(TypeSet, func.ntemp);
            std.mem.copy(TypeSet, types, params);

            var inst = InstanceAnalyzer{
                .mod = self,
                .func = func,
                .types = types,
            };
            gop.value_ptr.* = try inst.analyze();
        }

        return gop.value_ptr.ret;
    }

    // TODO: caching?
    fn global(self: *ModuleAnalyzer, id: ir.GlobalId) Error!Type {
        const glo = &self.mod.globals[@enumToInt(id)];
        switch (glo.*) {
            .func => |*func| return Type{ .func = .{
                .mod = self.mod,
                .func = func,
            } },

            .extern_func => |func| {
                const params = try self.root.allocator.alloc(Type, func.params.len);
                for (func.params, 0..) |param, i| {
                    params[i] = try self.typeFromIr(param);
                }

                const ret = try self.root.allocator.create(Type);
                ret.* = try self.typeFromIr(func.ret);

                return Type{ .extern_func = .{
                    .params = params,
                    .ret = ret,

                    .name = func.name,
                    .mod = func.mod,
                } };
            },

            .import => |imp| {
                const glo_mod = self.root.modules.getPtr(imp.mod) orelse return error.InvalidCode;
                const glo_id = glo_mod.exports.get(imp.sym) orelse return error.InvalidCode;

                var mod = ModuleAnalyzer{
                    .root = self.root,
                    .mod = glo_mod,
                };
                return mod.global(glo_id);
            },

            .constant => |c| return Type{ .constant = c },
        }
    }

    fn typeFromIr(_: *ModuleAnalyzer, opt_ty: ?ir.Type) !Type {
        const ir_ty = opt_ty orelse return Type{ .void = {} };
        switch (ir_ty) {
            .name => unreachable, // TODO

            .base => |base| return @as(Type, switch (base) {
                .u8 => .{ .int = .{
                    .signed = false,
                    .size = 3,
                } },
                .u16 => .{ .int = .{
                    .signed = false,
                    .size = 4,
                } },
                .u32 => .{ .int = .{
                    .signed = false,
                    .size = 5,
                } },
                .u64 => .{ .int = .{
                    .signed = false,
                    .size = 6,
                } },

                .i8 => .{ .int = .{
                    .signed = true,
                    .size = 3,
                } },
                .i16 => .{ .int = .{
                    .signed = true,
                    .size = 4,
                } },
                .i32 => .{ .int = .{
                    .signed = true,
                    .size = 5,
                } },
                .i64 => .{ .int = .{
                    .signed = true,
                    .size = 6,
                } },

                .f32 => .{ .float = 5 },
                .f64 => .{ .float = 6 },

                .string => .string,
            }),
        }
    }
};

const InstanceAnalyzer = struct {
    mod: *ModuleAnalyzer,
    func: *const ir.Function,
    types: []TypeSet,

    fn analyze(self: *InstanceAnalyzer) !Instance {
        // TODO: order blocks based on dependencies. The way IR is generated will
        //       already do this for us, but for safety it might be good to check it.
        var ret = TypeSet{};
        for (self.func.blocks) |blk| {
            for (blk.insns) |insn| {
                try self.instruction(insn);
            }

            if (blk.term == .ret) {
                if (blk.term.ret) |t| {
                    const set = self.types[@enumToInt(t)];
                    for (set.keys()) |ty| {
                        try ret.put(self.mod.root.allocator, ty, {});
                    }
                } else {
                    try ret.put(self.mod.root.allocator, .void, {});
                }
            }
        }
        std.debug.assert(ret.count() > 0);

        return Instance{
            .func = self.func,
            .types = self.types,
            .ret = ret,
        };
    }

    fn instruction(self: *InstanceAnalyzer, insn: ir.Instruction) !void {
        var ret = TypeSet.Managed.init(self.mod.root.allocator);
        switch (insn.op) {
            .literal => |c| try ret.put(.{ .constant = c }, {}),
            .copy => |t| ret.unmanaged = self.types[@enumToInt(t)],
            .global => |g| try ret.put(try self.mod.global(g), {}),

            .call => |call| {
                const func_set = self.types[@enumToInt(call[0])];
                const args = call[1..];

                std.debug.assert(func_set.keys().len == 1); // TODO: indirect calls
                switch (func_set.keys()[0]) {
                    .func => |func| {
                        const params = try self.mod.root.allocator.alloc(TypeSet, call.len - 1);
                        defer self.mod.root.allocator.free(params);
                        for (args, 0..) |t, i| {
                            params[i] = self.types[@enumToInt(t)];
                        }

                        var mod = ModuleAnalyzer{
                            .root = self.mod.root,
                            .mod = func.mod,
                        };
                        ret.unmanaged = try mod.call(func.func, params);
                    },

                    .extern_func => |func| {
                        for (func.params, 0..) |expected, i| {
                            const set = self.types[@enumToInt(args[i])];
                            if (set.keys().len != 1) {
                                return error.TypeMismatch;
                            }

                            const actual = set.keys()[0];
                            if (!typesCompatible(expected, actual)) {
                                return error.TypeMismatch;
                            }
                        }
                        try ret.put(func.ret.*, {});
                    },

                    else => return error.TypeMismatch,
                }
            },

            .add => unreachable, // TODO
            .sub => unreachable, // TODO
            .mul => unreachable, // TODO
            .div => unreachable, // TODO

            .phi => |opts| for (opts) |t| {
                const set = self.types[@enumToInt(t)];
                for (set.keys()) |ty| {
                    try ret.put(ty, {});
                }
            },
        }
        self.types[@enumToInt(insn.result)] = ret.unmanaged;
    }
};

fn typesCompatible(expected: Type, actual: Type) bool {
    if (expected.eql(actual)) {
        return true;
    }

    if (std.meta.activeTag(expected) == actual) {
        switch (expected) {
            // Range check
            .int => |a| {
                const b = actual.int;
                if ((!a.signed and b.signed) or
                    a.size < b.size or
                    (a.size == b.size and a.signed and !b.signed))
                {
                    return false;
                }
            },

            // Size check
            .float => |f| if (f < actual.float) {
                return false;
            },

            // Name/module are irrelevant
            .extern_func => |af| {
                const bf = actual.extern_func;

                for (af.params, 0..) |param, i| {
                    if (!param.eql(bf.params[i])) return false;
                }

                return af.ret.eql(bf.ret.*);
            },

            else => return false,
        }
    } else if (actual == .constant) {
        switch (actual.constant) {
            .int => |n| if (expected != .int) {
                return false;
            } else {
                const umax = @as(i66, 1) << (@as(u7, 1) << expected.int.size);
                const max = if (expected.int.signed) umax >> 1 else umax;
                const min = if (expected.int.signed) -max else 0;
                if (n < min or n >= max) {
                    return false;
                }
            },

            .float => if (expected != .float) {
                return false;
            },

            .string => if (expected != .string) {
                return false;
            },
        }
    } else {
        std.debug.print("{} != {}\n", .{ expected, actual });
        return false;
    }

    return true;
}

pub const Error = error{
    InvalidCode,
    TypeMismatch,
    OutOfMemory,
};

pub const Instances = std.ArrayHashMapUnmanaged(Signature, Instance, Signature.Context, false);

/// A unique (with high probability) value identifying a given function instance.
/// Derived from the function IR and the argument types.
pub const Signature = struct {
    hash: [Blake3.digest_length]u8,
    const Blake3 = std.crypto.hash.Blake3;

    pub fn init(func: ir.Function, params: []const TypeSet) Signature {
        var hasher = Blake3.init(.{});

        hashFunc(&hasher, func);

        std.debug.assert(params.len == func.arity);
        for (params) |set| {
            hasher.update("param");
            for (set.keys()) |ty| {
                ty.hash(&hasher);
            }
        }

        var sig: Signature = undefined;
        hasher.final(&sig.hash);
        return sig;
    }

    /// Hash table context
    pub const Context = struct {
        pub fn hash(_: Context, sig: Signature) u32 {
            return std.mem.readIntNative(u32, sig.hash[0..4]);
        }
        pub fn eql(_: Context, a: Signature, b: Signature, _: usize) bool {
            return std.mem.eql(u8, &a.hash, &b.hash);
        }
    };
};

/// A typed instantiation of a function
pub const Instance = struct {
    func: *const ir.Function,
    types: []const TypeSet,
    ret: TypeSet,
};

pub const TypeSet = std.ArrayHashMapUnmanaged(Type, void, Type.Context, true);

pub fn fmtTypeSet(tys: TypeSet) std.fmt.Formatter(formatTypeSet) {
    return .{ .data = tys };
}
fn formatTypeSet(tys: TypeSet, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    for (tys.keys(), 0..) |ty, j| {
        if (j > 0) {
            try writer.print(" | ", .{});
        }
        try writer.print("{}", .{ty});
    }
}

pub const Type = union(enum) {
    void,

    constant: ir.Constant,

    int: struct {
        signed: bool,
        size: u3, // Width of int = 1 << size
    },
    float: u3, // Width of float = 1 << size
    string,

    // An untyped Tara function
    func: struct {
        mod: *const ir.Module,
        func: *const ir.Function,
    },
    // A typed extern function
    extern_func: struct {
        params: []const Type,
        ret: *const Type,

        name: []const u8,
        mod: []const u8,
    },

    pub fn format(self: Type, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .void, .string => try writer.writeAll(@tagName(self)),

            .constant => |c| try writer.print("const {}", .{c}),

            .int => |i| try writer.print("{c}{d}", .{
                if (i.signed) @as(u8, 'i') else 'u',
                @as(u8, 1) << i.size,
            }),

            .float => |size| try writer.print("f{d}", .{
                @as(u8, 1) << size,
            }),

            .func => |func| try writer.print("fn {d} ({*}.{s})", .{ func.func.arity, func.mod, func.func.name }),
            .extern_func => |func| {
                try writer.print("extern \"{}\" fn {s}", .{ std.zig.fmtEscapes(func.mod), func.name });
                for (func.params) |param| {
                    try writer.print(" {}", .{param});
                }
                try writer.print(" (-> {})", .{func.ret});
            },
        }
    }

    fn hash(self: Type, hasher: anytype) void {
        autoHash(hasher, std.meta.activeTag(self));
        switch (self) {
            .void, .string => {},
            .int => |i| autoHash(hasher, i),
            .float => |f| autoHash(hasher, f),

            .constant => |c| switch (c) {
                .int => |i| autoHash(hasher, i),
                .float => |f| autoHash(hasher, @bitCast(u64, f)),
                .string => |s| {
                    autoHash(hasher, s.len);
                    hasher.update(s);
                },
            },

            .func => |f| hashFunc(hasher, f.func.*),
            .extern_func => |f| {
                f.ret.hash(hasher);
                for (f.params) |ty| {
                    hasher.update("param");
                    ty.hash(hasher);
                }
            },
        }
    }

    fn eql(a: Type, b: Type) bool {
        if (std.meta.activeTag(a) != b) return false;
        switch (a) {
            .void, .string => return true,
            .int, .float, .func => return std.meta.eql(a, b),

            .constant => |c| return c.eql(b.constant),

            .extern_func => |af| {
                const bf = b.extern_func;

                // Cheap pointer compare
                if (std.meta.eql(af, bf)) return true;

                // Expensive recursive compare
                if (!af.ret.eql(bf.ret.*)) return false;
                for (af.params, 0..) |param, i| {
                    if (!param.eql(bf.params[i])) return false;
                }

                if (!std.mem.eql(u8, af.name, bf.name)) return false;
                if (!std.mem.eql(u8, af.mod, bf.mod)) return false;

                return true;
            },
        }
    }

    /// Hash table context
    const Context = struct {
        pub fn hash(_: Context, ty: Type) u32 {
            var hasher = std.hash.Wyhash.init(0);
            ty.hash(&hasher);
            return @truncate(u32, hasher.final());
        }
        pub fn eql(_: Context, a: Type, b: Type, _: usize) bool {
            return a.eql(b);
        }
    };
};

fn hashFunc(hasher: anytype, func: ir.Function) void {
    // TODO: avoid recomputing hash for the same function multiple times
    for (func.blocks) |blk| {
        hasher.update("block");
        for (blk.insns) |insn| {
            hashInstruction(hasher, insn);
        }
        autoHash(hasher, blk.term);
    }
}

fn hashInstruction(hasher: anytype, insn: ir.Instruction) void {
    autoHash(hasher, std.meta.activeTag(insn.op));
    autoHash(hasher, insn.result);
    switch (insn.op) {
        .literal => |c| switch (c) {
            .int => |i| autoHash(hasher, i),
            .float => |f| autoHash(hasher, @bitCast(u64, f)),
            .string => |s| {
                autoHash(hasher, s.len);
                hasher.update(s);
            },
        },
        .copy => |t| autoHash(hasher, t),
        .global => |g| autoHash(hasher, g),

        .call => |ts| for (ts) |t| {
            autoHash(hasher, t);
        },

        .add => |ts| autoHash(hasher, ts),
        .sub => |ts| autoHash(hasher, ts),
        .mul => |ts| autoHash(hasher, ts),
        .div => |ts| autoHash(hasher, ts),

        .phi => |ts| for (ts) |t| {
            autoHash(hasher, t);
        },
    }
}

const autoHash = std.hash.autoHash;
