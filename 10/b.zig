const std = @import("std");

fn readLine(file: std.fs.File, buf: []u8) !?[]u8 {
    return file.reader().readUntilDelimiter(buf, '\n') catch |err| switch (err) {
        error.EndOfStream => return null,
        else => |e| return e,
    };
}

fn compare(ctx: void, a: u64, b: u64) bool {
    _ = ctx;
    return a < b;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    const file = try std.fs.cwd().openFile("input", .{});
    defer file.close();

    var buf: [128]u8 = undefined;

    var scores = std.ArrayListUnmanaged(u64) {};
    defer scores.deinit(allocator);

    outer:
    while (try readLine(file, &buf)) |line| {
        var stack = std.ArrayListUnmanaged(u8) {};
        defer stack.deinit(allocator);
        for (line) |char| {
            switch (char) {
                '(', '[', '{', '<' => try stack.append(allocator, char),
                ')' => if (stack.pop() != '(') continue :outer,
                ']' => if (stack.pop() != '[') continue :outer,
                '}' => if (stack.pop() != '{') continue :outer,
                '>' => if (stack.pop() != '<') continue :outer,
                else => {},
            }
        }
        var score: u64 = 0;
        while (stack.popOrNull()) |char| {
            score *= 5;
            score += switch (char) {
                '(' => @as(u64, 1),
                '[' => @as(u64, 2),
                '{' => @as(u64, 3),
                '<' => @as(u64, 4),
                else => unreachable,
            };
        }
        if (score != 0) {
            try scores.append(allocator, score);
        }
    }

    std.sort.sort(u64, scores.items, {}, compare);
    std.debug.print("{}\n", .{scores.items[scores.items.len / 2]});
}
