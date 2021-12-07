const std = @import("std");

fn compare(ctx: void, a: u16, b: u16) bool {
    _ = ctx;
    return a < b;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    const file = try std.fs.cwd().openFile("input", .{});
    defer file.close();

    var positions = std.ArrayListUnmanaged(u16) {};
    defer positions.deinit(allocator);

    var number: u16 = 0;
    while (true) switch (try file.reader().readByte()) {
        '0'...'9' => |c| {
            number *= 10;
            number += c - '0';
        },
        ',' => {
            try positions.append(allocator, number);
            number = 0;
        },
        '\n' => {
            try positions.append(allocator, number);
            break;
        },
        else => {},
    };

    std.sort.sort(u16, positions.items, {}, compare);

    const median = positions.items[positions.items.len / 2];
    var fuel_cost: u32 = 0;
    for (positions.items) |position| {
        if (position > median) {
            fuel_cost += position - median;
        } else {
            fuel_cost += median - position;
        }
    }

    std.debug.print("{}\n", .{fuel_cost});
}
