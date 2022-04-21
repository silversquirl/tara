//! An untyped intermediate representation using SSA form

const std = @import("std");

pub const Module = struct {
    arena: std.heap.ArenaAllocator,
    deps: []const []const u8, // Module names
    exports: std.StringHashMapUnmanaged(*const Global),
};

pub const Global = union(enum) {
    func: Function,
    extern_func: ExternFunction,
    import: Import,
    constant: Constant,

    pub fn format(self: Global, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (std.mem.eql(u8, fmt, "=")) {
            switch (self) {
                .func => |func| try writer.print("{}", .{func}),
                .extern_func => |func| try writer.print("{}", .{func}),
                .import => |imp| try writer.print("use {s} [{s}]", .{ imp.mod, imp.sym }),
                .constant => |c| try writer.print("{}", .{c}),
            }
        } else {
            switch (self) {
                .func => |func| try writer.writeAll(func.name),
                .extern_func => |func| try writer.writeAll(func.name),
                .import => |imp| try writer.print("{s}.{s}", .{ imp.mod, imp.sym }),
                .constant => |c| try writer.print("{}", .{c}),
            }
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
    arity: u32 = 0,
    variadic: bool = false,
    entry: *const Block,

    pub fn format(self: Function, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const ellipsis: []const u8 = if (self.variadic) "..." else "";
        try writer.print("fn {d}{s} {s}\n", .{ self.arity, ellipsis, self.name });

        try printBlocks(writer, self.entry);
    }
    fn printBlocks(writer: anytype, blk: *const Block) @TypeOf(writer).Error!void {
        try writer.print("{}", .{blk});
        switch (blk.term) {
            .jmp => |next| try printBlocks(writer, next),
            .br => |br| {
                try printBlocks(writer, br.z);
                try printBlocks(writer, br.nz);
            },
            .ret, .unr => {},
        }
    }
};

pub const Block = struct {
    insns: []const Instruction,
    term: Terminal,

    pub const Terminal = union(enum) {
        // Unconditional jump
        jmp: *const Block,
        // Conditional branch
        br: struct {
            cond: Temporary,
            z: *const Block,
            nz: *const Block,
        },
        // Return
        ret: ?Temporary,
        // Unreachable
        unr: void,

        pub fn format(self: Terminal, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (self) {
                .jmp => |next| try writer.print("jmp {:};", .{next}),
                .br => |br| try writer.print("br {}, {:}, {:};", .{ br.cond, br.z, br.nz }),
                .ret => |maybe_value| if (maybe_value) |v| {
                    try writer.print("ret {};", .{v});
                } else {
                    try writer.writeAll("ret;");
                },
                .unr => try writer.writeAll("unr;"),
            }
        }
    };

    pub fn format(self: *const Block, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (std.mem.eql(u8, fmt, ":")) {
            try writer.print(":{x}", .{@ptrToInt(self)});
        } else {
            try writer.print("  {x}:\n", .{@ptrToInt(self)});
            for (self.insns) |insn| {
                try writer.print("    {}\n", .{insn});
            }
            try writer.print("    {}\n", .{self.term});
        }
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
        global: *const Global,

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
