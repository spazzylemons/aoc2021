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

fn propagateEnergy(allocator: *std.mem.Allocator, cells: [][]u8) !usize {
    var queue = std.TailQueue([2]usize) {};
    defer while (queue.popFirst()) |node| allocator.destroy(node);
    var flashes: usize = 0;

    // increment all, and add to the queue any that will flash
    for (cells) |line, y| {
        for (line) |*cell, x| {
            if (cell.* == '9') {
                try appendPoint(allocator, &queue, x, y);
            } else {
                cell.* += 1;
            }
        }
    }
    // repeatedly propagate flash energy
    while (queue.popFirst()) |node| {
        const x = node.data[0];
        const y = node.data[1];
        allocator.destroy(node);
        if (cells[y][x] == '0') continue;
        cells[y][x] += 1;
        if (cells[y][x] <= '9') continue;
        cells[y][x] = '0';
        flashes += 1;
        const l = x > 0;
        const r = x < cells[y].len - 1;
        const u = y > 0;
        const d = y < cells.len - 1;
        if (l and u) try appendPoint(allocator, &queue, x - 1, y - 1);
        if (l      ) try appendPoint(allocator, &queue, x - 1, y    );
        if (l and d) try appendPoint(allocator, &queue, x - 1, y + 1);
        if (      d) try appendPoint(allocator, &queue, x,     y + 1);
        if (r and d) try appendPoint(allocator, &queue, x + 1, y + 1);
        if (r      ) try appendPoint(allocator, &queue, x + 1, y    );
        if (r and u) try appendPoint(allocator, &queue, x + 1, y - 1);
        if (      u) try appendPoint(allocator, &queue, x,     y - 1);
    }
    return flashes;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    const file = try std.fs.cwd().openFile("input", .{});
    defer file.close();

    var cells = std.ArrayListUnmanaged([]u8) {};
    defer {
        for (cells.items) |line| allocator.free(line);
        cells.deinit(allocator);
    }

    var buf: [128]u8 = undefined;
    var octopus_count: usize = 0;

    while (try readLine(file, &buf)) |line| {
        const slice = try cells.addOne(allocator);
        slice.* = &.{};
        slice.* = try allocator.dupe(u8, line);
        octopus_count += line.len;
    }

    var step: usize = 1;
    while (true) : (step += 1) {
        if (octopus_count == try propagateEnergy(allocator, cells.items)) {
            break;
        }
    }

    std.debug.print("{}\n", .{step});
}
