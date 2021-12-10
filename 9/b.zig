const std = @import("std");

fn readLine(file: std.fs.File, buf: []u8) !?[]u8 {
    return file.reader().readUntilDelimiter(buf, '\n') catch |err| switch (err) {
        error.EndOfStream => return null,
        else => |e| return e,
    };
}

fn appendPoint(allocator: *std.mem.Allocator, queue: *std.TailQueue([2]usize), x: usize, y: usize) !void {
    const node = try allocator.create(@TypeOf(queue.*).Node);
    queue.append(node);
    node.data[0] = x;
    node.data[1] = y;
}

fn checkBasin(allocator: *std.mem.Allocator, cells: [][]u8, x0: usize, y0: usize) !usize {
    var queue = std.TailQueue([2]usize) {};
    defer while (queue.popFirst()) |node| allocator.destroy(node);
    var counter: usize = 0;

    try appendPoint(allocator, &queue, x0, y0);

    while (queue.popFirst()) |node| {
        const x = node.data[0];
        const y = node.data[1];
        allocator.destroy(node);
        if (cells[y][x] == '9') continue;
        cells[y][x] = '9';
        counter += 1;
        if (x > 0) try appendPoint(allocator, &queue, x - 1, y);
        if (x < cells[y].len - 1) try appendPoint(allocator, &queue, x + 1, y);
        if (y > 0) try appendPoint(allocator, &queue, x, y - 1);
        if (y < cells.len - 1) try appendPoint(allocator, &queue, x, y + 1);
    }

    return counter;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    const file = try std.fs.cwd().openFile("input", .{});
    defer file.close();

    var heightmap = std.ArrayListUnmanaged([]u8) {};
    defer {
        for (heightmap.items) |line| allocator.free(line);
        heightmap.deinit(allocator);
    }

    var buf: [128]u8 = undefined;

    while (try readLine(file, &buf)) |line| {
        const slice = try heightmap.addOne(allocator);
        slice.* = &.{};
        slice.* = try allocator.dupe(u8, line);
    }

    var top_three_sizes = [_]usize { 0, 0, 0 };
    for (heightmap.items) |line, y| {
        for (line) |_, x| {
            const size = try checkBasin(allocator, heightmap.items, x, y);
            if (size > top_three_sizes[0]) {
                if (size > top_three_sizes[1]) {
                    top_three_sizes[0] = top_three_sizes[1];
                    if (size > top_three_sizes[2]) {
                        top_three_sizes[1] = top_three_sizes[2];
                        top_three_sizes[2] = size;
                    } else {
                        top_three_sizes[1] = size;
                    }
                } else {
                    top_three_sizes[0] = size;
                }
            }
        }
    }

    std.debug.print("{}\n", .{top_three_sizes[0] * top_three_sizes[1] * top_three_sizes[2]});
}
