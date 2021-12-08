const std = @import("std");

fn readLine(file: std.fs.File, buf: []u8) !?[]u8 {
    return file.reader().readUntilDelimiter(buf, '\n') catch |err| switch (err) {
        error.EndOfStream => return null,
        else => |e| return e,
    };
}

pub fn main() !void {
    const file = try std.fs.cwd().openFile("input", .{});
    defer file.close();

    var buf: [256]u8 = undefined;
    var easy_digit_count: usize = 0;

    while (try readLine(file, &buf)) |line| {
        var it = std.mem.split(u8, line, " ");
        var i: u8 = 0;
        while (i < 11) : (i += 1) {
            _ = it.next();
        }
        while (it.next()) |digit| {
            switch (digit.len) {
                2, 3, 4, 7 => easy_digit_count += 1,
                else => {},
            }
        }
    }
    std.debug.print("{}\n", .{easy_digit_count});
}
