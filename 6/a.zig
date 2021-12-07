const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    const file = try std.fs.cwd().openFile("input", .{});
    defer file.close();

    var lanternfish = std.ArrayListUnmanaged(u8) {};
    defer lanternfish.deinit(allocator);

    var number: u8 = 0;
    while (true) switch (try file.reader().readByte()) {
        '0'...'9' => |c| {
            number *= 10;
            number += c - '0';
        },
        ',' => {
            try lanternfish.append(allocator, number);
            number = 0;
        },
        '\n' => {
            try lanternfish.append(allocator, number);
            break;
        },
        else => {},
    };

    var day: u16 = 0;
    while (day < 80) : (day += 1) {
        var i: usize = 0;
        var len = lanternfish.items.len;
        while (i < len) : (i += 1) {
            if (lanternfish.items[i] == 0) {
                lanternfish.items[i] = 6;
                try lanternfish.append(allocator, 8);
            } else {
                lanternfish.items[i] -= 1;
            }
        }
    }

    std.debug.print("{}\n", .{lanternfish.items.len});
}
