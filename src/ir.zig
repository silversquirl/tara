//! An untyped intermediate representation using SSA form

const std = @import("std");

pub const Module = struct {
    arena: std.heap.ArenaAllocator,
    deps: []const []const u8, // Module names
    globals: []const Global,
    exports: std.StringHashMapUnmanaged(GlobalId),

    pub fn format(self: Module, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        for (self.globals) |glo, i| {
            try writer.print("${} := {}\n", .{ i, glo });
        }

        var it = self.exports.iterator();
        while (it.next()) |exp| {
            try writer.print("pub \"{}\" {}\n", .{ std.zig.fmtEscapes(exp.key_ptr.*), exp.value_ptr.* });
        }
    }
};

pub const Global = union(enum) {
    func: Function,
    extern_func: ExternFunction,
    import: Import,
    constant: Constant,

    pub fn format(self: Global, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .func => |func| try writer.print("{}", .{func}),
            .extern_func => |func| try writer.print("{}", .{func}),
            .import => |imp| try writer.print("use {s} [{s}]", .{ imp.mod, imp.sym }),
            .constant => |c| try writer.print("{}", .{c}),
        }
    }
};

pub const ExternFunction = struct {
    name: []const u8,
    mod: []const u8,
    params: []const Type,
    ret: ?Type,

    pub fn format(self: ExternFunction, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("extern \"{}\" fn {s}", .{ std.zig.fmtEscapes(self.mod), self.name });
        for (self.params) |param| {
            try writer.print(" {}", .{param});
        }
        if (self.ret) |ret| {
            try writer.print(" (-> {})", .{ret});
        }
    }
};

pub const Import = struct {
    mod: []const u8,
    sym: []const u8,
};

pub const Constant = union(enum) {
    int: i64,
    float: f64,
    string: []const u8,

    pub fn format(self: Constant, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .int => |i| try writer.print("{d}", .{i}),
            .float => |f| try writer.print("{d}", .{f}),
            .string => |s| try writer.print("\"{}\"", .{std.zig.fmtEscapes(s)}),
        }
    }
};

pub const Type = union(enum) {
    name: []const u8,
    base: Base,

    pub const Base = enum {
        u8,
        u16,
        u32,
        u64,

        i8,
        i16,
        i32,
        i64,

        f32,
        f64,

        string,
    };

    pub fn format(self: Type, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .name => |name| try writer.writeAll(name),
            .base => |base| try writer.writeAll(@tagName(base)),
        }
    }
};

pub const Function = struct {
    name: []const u8,
    arity: u32,
    variadic: bool = false,
    ntemp: u32, // Number of temporaries used by this function
    blocks: []const Block,

    pub fn format(self: Function, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const ellipsis: []const u8 = if (self.variadic) "..." else "";
        try writer.print("fn {d}{s} {s}\n", .{ self.arity, ellipsis, self.name });

        for (self.blocks) |blk, i| {
            try writer.print("  {}:\n{}", .{ i, blk });
        }
    }
};

pub const Block = struct {
    insns: []const Instruction,
    term: Terminal,

    pub const Terminal = union(enum) {
        // Unconditional jump
        jmp: BlockId,
        // Conditional branch
        br: struct {
            cond: Temporary,
            z: BlockId,
            nz: BlockId,
        },
        // Return
        ret: ?Temporary,
        // Unreachable
        unr: void,

        pub fn format(self: Terminal, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (self) {
                .jmp => |next| try writer.print("jmp {};", .{next}),
                .br => |br| try writer.print("br {}, {}, {};", .{ br.cond, br.z, br.nz }),
                .ret => |maybe_value| if (maybe_value) |v| {
                    try writer.print("ret {};", .{v});
                } else {
                    try writer.writeAll("ret;");
                },
                .unr => try writer.writeAll("unr;"),
            }
        }
    };

    pub fn format(self: Block, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        for (self.insns) |insn| {
            try writer.print("    {}\n", .{insn});
        }
        try writer.print("    {}\n", .{self.term});
    }
};
pub const Instruction = struct {
    result: Temporary,
    op: Op,

    pub const Op = union(enum) {
        int: i64,
        float: f64,
        str: []const u8,
        copy: Temporary,
        global: GlobalId,

        call: []const Temporary,

        add: [2]Temporary,
        sub: [2]Temporary,
        mul: [2]Temporary,
        div: [2]Temporary,

        phi: []const Temporary,
    };

    pub fn format(self: Instruction, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{} = ", .{self.result});
        switch (self.op) {
            .int => |i| try writer.print("{d}", .{i}),
            .float => |f| try writer.print("{d}", .{f}),
            .str => |s| try writer.print("\"{}\"", .{std.zig.fmtEscapes(s)}),
            .copy => |t| try writer.print("{}", .{t}),
            .global => |g| try writer.print("{}", .{g}),

            .call => |args| {
                try writer.print("{}(", .{args[0]});
                for (args[1..]) |arg, i| {
                    if (i > 0) {
                        try writer.writeAll(", ");
                    }
                    try writer.print("{}", .{arg});
                }
                try writer.writeByte(')');
            },

            .add => |args| try writer.print("{} + {}", .{ args[0], args[1] }),
            .sub => |args| try writer.print("{} - {}", .{ args[0], args[1] }),
            .mul => |args| try writer.print("{} * {}", .{ args[0], args[1] }),
            .div => |args| try writer.print("{} / {}", .{ args[0], args[1] }),

            .phi => |args| {
                try writer.writeAll("Φ(");
                for (args) |arg, i| {
                    if (i > 0) {
                        try writer.writeAll(", ");
                    }
                    try writer.print("{}", .{arg});
                }
                try writer.writeByte(')');
            },
        }
        try writer.writeByte(';');
    }
};

pub const Temporary = enum(u32) {
    _,

    pub fn format(self: Temporary, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("%{}", .{@enumToInt(self)});
    }
};

pub const BlockId = enum(u32) {
    _,
    pub fn format(self: BlockId, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print(":{}", .{@enumToInt(self)});
    }
};

pub const GlobalId = enum(u32) {
    _,
    pub fn format(self: GlobalId, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("${}", .{@enumToInt(self)});
    }
};
