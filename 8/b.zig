const std = @import("std");

fn readLine(file: std.fs.File, buf: []u8) !?[]u8 {
    return file.reader().readUntilDelimiter(buf, '\n') catch |err| switch (err) {
        error.EndOfStream => return null,
        else => |e| return e,
    };
}

const DigitDecoder = struct {
    numbers: [10][]const u8,
    num_remaining: usize = 10,
    remaining: [10]u4 = .{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
    num_resolved: usize = 0,
    resolved: [10]u4 = undefined,

    fn decode(self: *DigitDecoder) void {
        self.resolveWithLength(1, 2);
        self.resolveWithLength(4, 4);
        self.resolveWithLength(7, 3);
        self.resolveWithLength(8, 7);
        self.resolveCommon(3, 5);
        self.resolveZeroAndTwo();
        self.resolveWithLength(5, 5);
        self.resolveSixAndNine();
    }

    fn resolveWithLength(self: *DigitDecoder, digit: u4, len: u3) void {
        for (self.remaining[0..self.num_remaining]) |d, di| {
            if (self.numbers[d].len == len) {
                self.resolve(di, digit);
                return;
            }
        }
        @panic("bad puzzle input");
    }

    fn getCommonCount(num1: []const u8, num2: []const u8) u8 {
        var common_count: u8 = 0;
        for (num1) |c| {
            if (std.mem.indexOfScalar(u8, num2, c) != null) {
                common_count += 1;
            }
        }
        return common_count;
    }

    fn resolveCommon(self: *DigitDecoder, digit: u4, len: u3) void {
        var segs: [3][]const u8 = undefined;
        var inds: [3]usize = undefined;
        var i: u8 = 0;
        for (self.remaining[0..self.num_remaining]) |d, di| {
            const num = self.numbers[d];
            if (num.len == len) {
                segs[i] = num;
                inds[i] = di;
                i += 1;
                if (i == 3) break;
            }
        }
        const goal = len - 1;
        if (getCommonCount(segs[0], segs[1]) == goal) {
            if (getCommonCount(segs[0], segs[2]) == goal) {
                self.resolve(inds[0], digit);
            } else {
                self.resolve(inds[1], digit);
            }
        } else {
            self.resolve(inds[2], digit);
        }
    }

    fn resolveFiveAndSix(self: *DigitDecoder) void {
        // 5 is a subset of 6
        for (self.remaining[0..self.num_remaining]) |d, di| {
            const num1 = self.numbers[d];
            if (num1.len != 5) continue;
            for (self.remaining[0..self.num_remaining]) |e, ei| {
                const num2 = self.numbers[e];
                if (num2.len != 6) continue;
                if (isSubset(num1, num2)) {
                    if (ei > di) {
                        self.resolve(ei, 6);
                        self.resolve(di, 5);
                    } else {
                        self.resolve(di, 5);
                        self.resolve(ei, 6);
                    }
                    return;
                }
            }
        }
        @panic("bad puzzle input");
    }

    fn resolveZeroAndTwo(self: *DigitDecoder) void {
        // out of what is left, 2 has two unique segments and 0 has one
        var counts = std.mem.zeroes([5]u2);
        for ("abcdefg") |c| {
            var contains = std.mem.zeroes([5]bool);
            var contain_count: u8 = 0;
            for (self.remaining[0..self.num_remaining]) |d, di| {
                if (std.mem.indexOfScalar(u8, self.numbers[d], c) != null) {
                    contains[di] = true;
                    contain_count += 1;
                }
            }
            if (contain_count == 4) {
                for (contains) |contain, conti| {
                    if (!contain) {
                        counts[conti] += 1;
                    }
                }
            }
        }
        var onei: usize = undefined;
        var twoi: usize = undefined;
        for (counts) |count, counti| {
            if (count == 2) {
                twoi = counti;
            } else if (count == 1) {
                onei = counti;
            }
        }
        if (twoi > onei) {
            self.resolve(twoi, 2);
            self.resolve(onei, 0);
        } else {
            self.resolve(onei, 0);
            self.resolve(twoi, 2);
        }
    }

    fn isSubset(subset: []const u8, superset: []const u8) bool {
        for (subset) |c| {
            if (std.mem.indexOfScalar(u8, superset, c) == null) return false;
        }
        return true;
    }

    fn resolveSixAndNine(self: *DigitDecoder) void {
        const four = self.numbers[self.resolved[4]];
        if (isSubset(four, self.numbers[self.remaining[0]])) {
            self.resolve(1, 6);
            self.resolve(0, 9);
        } else {
            self.resolve(1, 9);
            self.resolve(0, 6);
        }
    }

    fn resolve(self: *DigitDecoder, num_index: usize, digit: u4) void {
        self.resolved[digit] = self.remaining[num_index];
        self.remaining[num_index] = self.remaining[self.num_remaining - 1];
        self.num_remaining -= 1;
        self.num_resolved += 1;
    }

    fn find(self: *DigitDecoder, num1: []const u8) u4 {
        for (self.resolved) |resolved, ri| {
            const num2 = self.numbers[resolved];
            if (num1.len != num2.len) continue;
            var i: usize = 0;
            while (i < num1.len) : (i += 1) {
                if (std.mem.indexOfScalar(u8, num1, num2[i]) == null) break;
                if (std.mem.indexOfScalar(u8, num2, num1[i]) == null) break;
            } else {
                return @intCast(u4, ri);
            }
        }
        @panic("bad puzzle input");
    }
};

pub fn main() !void {
    const file = try std.fs.cwd().openFile("input", .{});
    defer file.close();

    var buf: [256]u8 = undefined;
    var sum: u64 = 0;

    while (try readLine(file, &buf)) |line| {
        var it = std.mem.split(u8, line, " ");
        var decoder = DigitDecoder { .numbers = undefined };
        for (decoder.numbers) |*number| {
            number.* = it.next().?;
        }
        decoder.decode();
        _ = it.next();
        var i: u16 = 0;
        while (it.next()) |n| {
            i *= 10;
            i += decoder.find(n);
        }
        sum += i;
    }

    std.debug.print("{}\n", .{sum});
}
