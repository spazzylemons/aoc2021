const std = @import("std");

const CommandType = enum { Forward, Down, Up };

const command_map = std.ComptimeStringMap(CommandType, .{
    .{ "forward", .Forward },
    .{ "down", .Down },
    .{ "up", .Up },
});

fn readLine(file: std.fs.File, buf: []u8) !?[]u8 {
    return file.reader().readUntilDelimiter(buf, '\n') catch |err| switch (err) {
        error.EndOfStream => return null,
        else => |e| return e,
    };
}

const Submarine = struct {
    hPos: u32 = 0,
    depth: u32 = 0,
    aim: u32 = 0,

    fn nextCommand(sub: *Submarine, slice: []const u8) !void {
        var it = std.mem.split(u8, slice, " ");
        const command = command_map.get(it.next().?).?;
        const amount = try std.fmt.parseInt(u32, it.next().?, 10);
        switch (command) {
            .Forward => {
                sub.hPos += amount;
                sub.depth += sub.aim * amount;
            },
            .Down => sub.aim += amount,
            .Up => sub.aim -= amount,
        }
    }
};

pub fn main() !void {
    const file = try std.fs.cwd().openFile("../puzzle3/input", .{});
    defer file.close();

    var sub = Submarine {};
    var buf: [128]u8 = undefined;

    while (try readLine(file, &buf)) |slice| {
        try sub.nextCommand(slice);
    }

    std.debug.print("{}\n", .{sub.hPos * sub.depth});
}
