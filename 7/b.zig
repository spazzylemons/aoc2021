const std = @import("std");

fn getFuelCost(positions: []u16, test_position: u16) u64 {
    var fuel_cost: u64 = 0;
    for (positions) |position| {
        const distance = if (position > test_position)
            @as(u64, position - test_position)
        else
            @as(u64, test_position - position);
        fuel_cost += (distance * (distance + 1)) / 2;
    }
    return fuel_cost;
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
    var min_position: u16 = 0;
    var max_position: u16 = 0;
    while (true) switch (try file.reader().readByte()) {
        '0'...'9' => |c| {
            number *= 10;
            number += c - '0';
        },
        ',' => {
            try positions.append(allocator, number);
            min_position = std.math.min(min_position, number);
            max_position = std.math.max(max_position, number);
            number = 0;
        },
        '\n' => {
            try positions.append(allocator, number);
            min_position = std.math.min(min_position, number);
            max_position = std.math.max(max_position, number);
            break;
        },
        else => {},
    };

    var best_fuel_cost: u64 = 0;
    while (min_position != max_position) {
        // test min and max positions
        const min_pos_fuel_cost = getFuelCost(positions.items, min_position);
        const max_pos_fuel_cost = getFuelCost(positions.items, max_position);
        // choose which half to continue searching in
        const mid_position = min_position + ((max_position - min_position) / 2);
        if (min_pos_fuel_cost < max_pos_fuel_cost) {
            best_fuel_cost = min_pos_fuel_cost;
            max_position = mid_position;
        } else {
            best_fuel_cost = max_pos_fuel_cost;
            min_position = mid_position + 1;
        }
    }

    std.debug.print("{}\n", .{best_fuel_cost});
}
