const std = @import("std");

fn skipLines(file: std.fs.File, n: usize) !void {
    var i: usize = 0;
    while (i < n) : (i += 1) {
        try file.reader().skipUntilDelimiterOrEof('\n');
    }
}

fn getParam(file: std.fs.File) !i64 {
    var buf: [16]u8 = undefined;
    const line = try file.reader().readUntilDelimiter(&buf, '\n');
    return try std.fmt.parseInt(i64, line[6..], 10);
}

fn calcZForDigit(z: i64, digit: u8, params: [3]i64) i64 {
    if (@rem(z, 26) + params[1] != digit) {
        return @divTrunc((z * 26) + digit + params[2], params[0]);
    } else {
        return @divTrunc(z, params[0]);
    }
}

const LENGTH = 14;

const State = struct { z: i64, index: u8 };

const Cache = std.AutoHashMap(State, void);

const Searcher = struct {
    cache: Cache,
    params: [LENGTH][3]i64,
    buf: [LENGTH]u8,

    fn findMin(self: *Searcher, z: i64, index: u8) std.mem.Allocator.Error!bool {
        if (self.cache.contains(.{ .z = z, .index = index })) {
            return false;
        } else if (index == LENGTH) {
            return z == 0;
        } else {
            var digit: u8 = 1;
            while (digit <= 9) : (digit += 1) {
                if (try self.findMin(calcZForDigit(z, digit, self.params[index]), index + 1)) {
                    self.buf[index] = digit + '0';
                    return true;
                }
            }
            try self.cache.put(.{ .z = z, .index = index }, {});
            return false;
        }
    }

    fn findMax(self: *Searcher, z: i64, index: u8) std.mem.Allocator.Error!bool {
        if (self.cache.contains(.{ .z = z, .index = index })) {
            return false;
        } else if (index == LENGTH) {
            return z == 0;
        } else {
            var digit: u8 = 9;
            while (digit >= 1) : (digit -= 1) {
                if (try self.findMax(calcZForDigit(z, digit, self.params[index]), index + 1)) {
                    self.buf[index] = digit + '0';
                    return true;
                }
            }
            try self.cache.put(.{ .z = z, .index = index }, {});
            return false;
        }
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("/home/nil/projects/jpstuff/input24.txt", .{});
    defer file.close();

    var searcher: Searcher = undefined;

    for (searcher.params) |*param| {
        try skipLines(file, 4);
        param[0] = try getParam(file);
        param[1] = try getParam(file);
        try skipLines(file, 9);
        param[2] = try getParam(file);
        try skipLines(file, 2);
    }

    searcher.cache = Cache.init(allocator);
    defer searcher.cache.deinit();

    if (!try searcher.findMax(0, 0)) {
        @panic("no max number");
    }
    std.debug.print("part one: {s}\n", .{searcher.buf});
    if (!try searcher.findMin(0, 0)) {
        @panic("no min number");
    }
    std.debug.print("part two: {s}\n", .{searcher.buf});
}
