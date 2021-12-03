const std = @import("std");

pub fn main() !void {
    const file = try std.fs.cwd().openFile("input", .{});
    defer file.close();

    var buf: [128]u8 = undefined;

    var prev: ?u32 = null;
    var increase_count: usize = 0;

    while (true) {
        const num_slice = file.reader().readUntilDelimiter(&buf, '\n') catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };
        const num = try std.fmt.parseInt(u32, num_slice, 10);
        if (prev) |p| {
            if (p < num) {
                increase_count += 1;
            }
        }
        prev = num;
    }

    std.debug.print("{}\n", .{increase_count});
}
