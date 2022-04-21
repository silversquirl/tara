const std = @import("std");
const bexpr = @import("bexpr");
const genir = @import("genir.zig");
const ir = @import("ir.zig");

pub fn main() !u8 {
    if (errorMain()) |_| {
        return 0;
    } else |err| {
        switch (err) {
            error.ParseError => {},
            error.OutOfMemory => std.debug.print("Out of memory\n", .{}),
            error.NotEnoughArgs, error.TooManyArgs => std.debug.print(
                \\Usage: tara FILE
                \\
            , .{}),
            else => |e| return e,
        }
        return 1;
    }
}

fn errorMain() !void {
    var args = try std.process.argsWithAllocator(std.heap.page_allocator);
    defer args.deinit();
    std.debug.assert(args.skip());

    const path = args.next() orelse {
        return error.NotEnoughArgs;
    };
    if (args.skip()) {
        return error.TooManyArgs;
    }

    var modules = std.StringHashMap(ir.Module).init(std.heap.page_allocator);
    try loadModuleFile(&modules, "main", path);
    {
        var it = modules.iterator();
        while (it.next()) |mod| {
            std.debug.print("===== {s} =====\n\n", .{mod.key_ptr.*});
            var it2 = mod.value_ptr.exports.iterator();
            while (it2.next()) |exp| {
                std.debug.print("{s} := {=}\n", .{ exp.key_ptr.*, exp.value_ptr.* });
            }
            std.debug.print("\n", .{});
        }
    }
}

fn loadModuleFile(modules: *std.StringHashMap(ir.Module), module: []const u8, path: []const u8) anyerror!void {
    const gop = try modules.getOrPut(module);
    if (gop.found_existing) return;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const ast = try loadFile(arena.allocator(), path);
    var gen = genir.Generator.init(std.heap.page_allocator);
    gop.value_ptr.* = try gen.module(ast);

    for (gop.value_ptr.deps) |dep| {
        try loadModule(modules, dep);
    }
}
fn loadModule(modules: *std.StringHashMap(ir.Module), module: []const u8) !void {
    var path_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const path = try std.fmt.bufPrint(&path_buf, "std/{s}.tara", .{module});
    try loadModuleFile(modules, module, path);
}

fn loadFile(allocator: std.mem.Allocator, path: []const u8) ![]const bexpr.Value {
    const source = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, path, std.math.maxInt(u32));
    defer std.heap.page_allocator.free(source);

    var p = bexpr.Parser.init(allocator, source);
    return p.read() catch |err| {
        if (err == error.ParseError) {
            std.debug.print("Parse error: {}\n", .{p.err});
        }
        return err;
    };
}

test "basic test" {
    try std.testing.expectEqual(10, 3 + 7);
}
