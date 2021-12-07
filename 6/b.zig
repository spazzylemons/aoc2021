const std = @import("std");

pub fn main() !void {
    const file = try std.fs.cwd().openFile("input", .{});
    defer file.close();

    var lanterfish_counts = [1]u64{0} ** 9;

    var number: u8 = 0;
    while (true) switch (try file.reader().readByte()) {
        '0'...'9' => |c| {
            number *= 10;
            number += c - '0';
        },
        ',' => {
            lanterfish_counts[number] += 1;
            number = 0;
        },
        '\n' => {
            lanterfish_counts[number] += 1;
            break;
        },
        else => {},
    };

    var day: u16 = 0;
    while (day < 256) : (day += 1) {
        const producing = lanterfish_counts[0];
        var i: usize = 1;
        while (i < 9) : (i += 1) {
            lanterfish_counts[i - 1] = lanterfish_counts[i];
        }
        lanterfish_counts[6] += producing;
        lanterfish_counts[8] = producing;
    }

    var sum: u64 = 0;
    for (lanterfish_counts) |count| {
        sum += count;
    }
    std.debug.print("{}\n", .{sum});
}
