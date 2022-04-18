const std = @import("std");
const bexpr = @import("bexpr");

pub fn main() u8 {
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
            else => |e| std.debug.print("{s}\n", .{@errorName(e)}),
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

    const ast = try loadFile(path);
    _ = ast;
}

fn loadFile(path: []const u8) ![]const bexpr.Value {
    const source = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, path, 1 << 32);
    defer std.heap.page_allocator.free(source);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var p = bexpr.Parser.init(arena.allocator(), source);
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
