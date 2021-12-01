const std = @import("std");

fn readNextNumber(file: std.fs.File) !?u32 {
    var buf: [128]u8 = undefined;
    const num_slice = file.reader().readUntilDelimiter(&buf, '\n') catch |err| switch (err) {
        error.EndOfStream => return null,
        else => |e| return e,
    };
    return try std.fmt.parseInt(u32, num_slice, 10);
}

pub fn main() !void {
    const file = try std.fs.cwd().openFile("../puzzle1/input", .{});
    defer file.close();

    var window = [3]?u32 { null, null, null };
    var prev: ?u32 = undefined;
    var increase_count: usize = 0;

    while (try readNextNumber(file)) |num| {
        // slide down the window
        window[0] = window[1];
        window[1] = window[2];
        window[2] = num;
        if (window[0] != null and window[1] != null) {
            const sum = window[0].? + window[1].? + num;
            if (prev) |p| {
                if (sum > p) {
                    increase_count += 1;
                }
            }
            prev = sum;
        }
    }

    std.debug.print("{}\n", .{increase_count});
}
