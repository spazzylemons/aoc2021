const std = @import("std");

const dice_rolls = blk: {
    var rolls: [27]u8 = undefined;
    var i = 0;
    // calculate dice rolls
    var a = 1;
    while (a <= 3) : (a += 1) {
        var b = 1;
        while (b <= 3) : (b += 1) {
            var c = 1;
            while (c <= 3) : (c += 1) {
                rolls[i] = a + b + c;
                i += 1;
            }
        }
    }
    break :blk rolls;
};

const Cache = [10][21][10][21][2]u64;

fn game(p1_pos: u8, p1_score: u8, p2_pos: u8, p2_score: u8, cache: *Cache) [2]u64 {
    const cached = cache[p1_pos][p1_score][p2_pos][p2_score];
    if (cached[0] != 0 or cached[1] != 0) {
        return cached;
    } else {
        var wins = [2]u64{ 0, 0 };
        for (dice_rolls) |roll| {
            const new_pos = (p1_pos + roll) % 10;
            const new_score = p1_score + new_pos + 1;
            if (new_score >= 21) {
                wins[0] += 1;
            } else {
                const new_wins = game(p2_pos, p2_score, new_pos, new_score, cache);
                wins[1] += new_wins[0];
                wins[0] += new_wins[1];
            }
        }
        cache[p1_pos][p1_score][p2_pos][p2_score] = wins;
        return wins;
    }
}

fn readInitialState(filename: []const u8) ![2]u8 {
    var buf: [64]u8 = undefined;

    const input = blk: {
        const file = try std.fs.cwd().openFile(filename, .{});
        defer file.close();
        break :blk buf[0..try file.reader().readAll(&buf)];
    };

    var result: [2]u8 = undefined;

    var lines = std.mem.split(u8, input, "\n");
    for (result) |*pos| {
        const line = lines.next().?;
        const space = std.mem.lastIndexOfScalar(u8, line, ' ').? + 1;
        pos.* = (try std.fmt.parseUnsigned(u8, line[space..], 10)) - 1;
    }

    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    const pos = try readInitialState("input");

    const cache = try allocator.create(Cache);
    defer allocator.destroy(cache);

    @memset(@ptrCast([*]u8, cache), 0, @sizeOf(Cache));

    const wins = game(pos[0], 0, pos[1], 0, cache);

    std.debug.print("{}\n", .{std.math.max(wins[0], wins[1])});
}
