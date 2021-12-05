const std = @import("std");

const PointMap = std.AutoHashMap([2]u16, u16);

fn readLine(file: std.fs.File, buf: []u8) !?[]u8 {
    return file.reader().readUntilDelimiter(buf, '\n') catch |err| switch (err) {
        error.EndOfStream => return null,
        else => |e| return e,
    };
}

fn markPoint(p: [2]u16, map: *PointMap, counter: *u16) !void {
    const gop = try map.getOrPut(p);
    if (gop.found_existing) {
        gop.value_ptr.* += 1;
        if (gop.value_ptr.* == 2) {
            counter.* += 1;
        }
    } else {
        gop.value_ptr.* = 1;
    }
}

fn orderSwap(lo: *u16, hi: *u16) void {
    if (lo.* > hi.*) {
        const temp = lo.*;
        lo.* = hi.*;
        hi.* = temp;
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    const file = try std.fs.cwd().openFile("input", .{});
    defer file.close();

    var points = PointMap.init(allocator);
    defer points.deinit();
    var counter: u16 = 0;

    var buf: [64]u8 = undefined;
    while (try readLine(file, &buf)) |slice| {
        const i = std.mem.indexOf(u8, slice, ",").?;
        const j = std.mem.indexOf(u8, slice[i..], " -> ").? + i;
        const k = std.mem.indexOf(u8, slice[j..], ",").? + j;
        var x1 = try std.fmt.parseUnsigned(u16, slice[0..i], 10);
        var y1 = try std.fmt.parseUnsigned(u16, slice[i + 1..j], 10);
        var x2 = try std.fmt.parseUnsigned(u16, slice[j + 4..k], 10);
        var y2 = try std.fmt.parseUnsigned(u16, slice[k + 1..], 10);
        orderSwap(&x1, &x2);
        orderSwap(&y1, &y2);
        if (x1 == x2) {
            var y = y1;
            while (y <= y2) : (y += 1) {
                try markPoint(.{x1, y}, &points, &counter);
            }
        } else if (y1 == y2) {
            var x = x1;
            while (x <= x2) : (x += 1) {
                try markPoint(.{x, y1}, &points, &counter);
            }
        }
    }
    std.debug.print("{}\n", .{counter});
}
