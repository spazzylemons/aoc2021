const std = @import("std");

fn readLine(file: std.fs.File, buf: []u8) !?[]u8 {
    return file.reader().readUntilDelimiter(buf, '\n') catch |err| switch (err) {
        error.EndOfStream => return null,
        else => |e| return e,
    };
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

    var low_point_sum: u32 = 0;
    for (heightmap.items) |line, y| {
        for (line) |cell, x| {
            if (x > 0 and line[x - 1] <= cell) continue;
            if (x < line.len - 1 and line[x + 1] <= cell) continue;
            if (y > 0 and heightmap.items[y - 1][x] <= cell) continue;
            if (y < heightmap.items.len - 1 and heightmap.items[y + 1][x] <= cell) continue;
            low_point_sum += (cell - '0') + 1;
        }
    }

    std.debug.print("{}\n", .{low_point_sum});
}
