const std = @import("std");

fn readLine(file: std.fs.File, buf: []u8) !?[]u8 {
    return file.reader().readUntilDelimiter(buf, '\n') catch |err| switch (err) {
        error.EndOfStream => return null,
        else => |e| return e,
    };
}

fn readInput(allocator: std.mem.Allocator, lines: *std.ArrayList([]u8), filename: []const u8) !void {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var buf: [256]u8 = undefined;
    while (try readLine(file, &buf)) |slice| {
        const element = try lines.addOne();
        element.* = &.{};
        element.* = try allocator.dupe(u8, slice);
    }
}

const PendingMove = struct {
    src_y: usize,
    src_x: usize,
    dst_y: usize,
    dst_x: usize,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var lines = std.ArrayList([]u8).init(allocator);
    defer {
        for (lines.items) |line| allocator.free(line);
        lines.deinit();
    }

    try readInput(allocator, &lines, "input");

    var to_move = std.ArrayList(PendingMove).init(allocator);
    defer to_move.deinit();

    var i: u32 = 1;
    while (true) : (i += 1) {
        var moved = false;
        to_move.clearAndFree();
        for (lines.items) |line, y| {
            for (line) |cell, x| {
                if (cell == '>') {
                    const dst_x = (x + 1) % line.len;
                    if (line[dst_x] == '.') {
                        try to_move.append(.{
                            .src_y = y, .src_x = x,
                            .dst_y = y, .dst_x = dst_x,
                        });
                    }
                }
            }
        }
        if (to_move.items.len != 0) {
            moved = true;
        }
        for (to_move.items) |move| {
            lines.items[move.src_y][move.src_x] = '.';
            lines.items[move.dst_y][move.dst_x] = '>';
        }
        to_move.clearAndFree();
        for (lines.items) |line, y| {
            const dst_y = (y + 1) % lines.items.len;
            for (line) |cell, x| {
                if (cell == 'v') {
                    if (lines.items[dst_y][x] == '.') {
                        try to_move.append(.{
                            .src_y = y, .src_x = x,
                            .dst_y = dst_y, .dst_x = x,
                        });
                    }
                }
            }
        }
        if (to_move.items.len != 0) {
            moved = true;
        }
        for (to_move.items) |move| {
            lines.items[move.src_y][move.src_x] = '.';
            lines.items[move.dst_y][move.dst_x] = 'v';
        }
        if (!moved) {
            break;
        }
    }
    std.debug.print("{}\n", .{i});
}