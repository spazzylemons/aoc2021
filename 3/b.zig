const std = @import("std");

fn readLine(file: std.fs.File, buf: []u8) !?[]u8 {
    return file.reader().readUntilDelimiter(buf, '\n') catch |err| switch (err) {
        error.EndOfStream => return null,
        else => |e| return e,
    };
}

fn findCounts(values: []const []const u8, index: usize) [2]usize {
    var counts = [_]usize{ 0, 0 };
    for (values) |value| {
        counts[value[index] - '0'] += 1;
    }
    return counts;
}

fn findMostCommon(values: []const []const u8, index: usize) ?u8 {
    const counts = findCounts(values, index);
    if (counts[0] > counts[1]) {
        return '0';
    } else if (counts[1] > counts[0]) {
        return '1';
    } else {
        return null;
    }
}

fn findLeastCommon(values: []const []const u8, index: usize) ?u8 {
    if (findMostCommon(values, index)) |bit| {
        if (bit == '0') {
            return '1';
        } else {
            return '0';
        }
    }
    return null;
}

fn filterOut(list: *std.ArrayListUnmanaged([]u8), index: usize, bit: u8) void {
    var i: usize = 0;
    while (i < list.items.len) {
        if (list.items[i][index] != bit) {
            _ = list.swapRemove(i);
        } else {
            i += 1;
        }
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    const file = try std.fs.cwd().openFile("input", .{});
    defer file.close();

    var buf: [128]u8 = undefined;
    var values = std.ArrayListUnmanaged([]u8) {};
    defer {
        for (values.items) |v| allocator.free(v);
        values.deinit(allocator);
    }

    while (try readLine(file, &buf)) |slice| {
        try values.append(allocator, try allocator.dupe(u8, slice));
    }

    var copy = std.ArrayListUnmanaged([]u8) {};
    defer copy.deinit(allocator);
    var bit_index: usize = 0;

    try copy.appendSlice(allocator, values.items);
    while (copy.items.len > 1) {
        const most_common = findMostCommon(copy.items, bit_index) orelse '1';
        filterOut(&copy, bit_index, most_common);
        bit_index += 1;
    }
    const oxygen = try std.fmt.parseUnsigned(u64, copy.items[0], 2);
    copy.clearAndFree(allocator);

    try copy.appendSlice(allocator, values.items);
    bit_index = 0;
    while (copy.items.len > 1) {
        const least_common = findLeastCommon(copy.items, bit_index) orelse '0';
        filterOut(&copy, bit_index, least_common);
        bit_index += 1;
    }
    const co2 = try std.fmt.parseUnsigned(u64, copy.items[0], 2);

    std.debug.print("{}\n", .{oxygen * co2});
}
