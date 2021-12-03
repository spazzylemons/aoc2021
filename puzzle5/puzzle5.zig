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
    var counters: []usize = &.{};
    var total: usize = 0;
    defer allocator.free(counters);

    while (try readLine(file, &buf)) |slice| {
        if (counters.len == 0) {
            counters = try allocator.alloc(usize, slice.len);
            std.mem.set(usize, counters, 0);
        }
        for (slice) |c, i| {
            if (c == '1') counters[i] += 1;
        }
        total += 1;
    }

    var gamma: u64 = 0;
    var epsilon: u64 = 0;

    for (counters) |c| {
        gamma <<= 1;
        epsilon <<= 1;
        if (c * 2 > total) {
            gamma += 1;
        } else {
            epsilon += 1;
        }
    }

    std.debug.print("{}\n", .{gamma * epsilon});
}
