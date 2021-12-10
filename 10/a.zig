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

    var buf: [128]u8 = undefined;

    var score: u64 = 0;

    while (try readLine(file, &buf)) |line| {
        var stack = std.ArrayListUnmanaged(u8) {};
        defer stack.deinit(allocator);
        for (line) |char| {
            switch (char) {
                '(', '[', '{', '<' => try stack.append(allocator, char),
                ')' => if (stack.pop() != '(') {
                    score += 3;
                    break;
                },
                ']' => if (stack.pop() != '[') {
                    score += 57;
                    break;
                },
                '}' => if (stack.pop() != '{') {
                    score += 1197;
                    break;
                },
                '>' => if (stack.pop() != '<') {
                    score += 25137;
                    break;
                },
                else => {},
            }
        }
    }

    std.debug.print("{}\n", .{score});
}
