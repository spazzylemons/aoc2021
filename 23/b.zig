const std = @import("std");

const ROOM_SIZE = 4;
const HALLWAY_SIZE = 11;
const HALLWAY_POINTS = [_]u4{0, 1, 3, 5, 7, 9, 10};
const COSTS = [_]u16{1, 10, 100, 1000};

const Amphipod = enum(u3) {
    None = 0,
    A = 1,
    B = 2,
    C = 3,
    D = 4,

    fn toIndex(self: Amphipod) u3 {
        return @enumToInt(self) - 1;
    }

    fn fromIndex(index: u3) Amphipod {
        return @intToEnum(Amphipod, index + 1);
    }
};

fn optMin(a: ?u16, b: ?u16) ?u16 {
    if (a == null) return b;
    if (b == null) return a;
    return std.math.min(a.?, b.?);
}

fn optAdd(a: ?u16, b: ?u16) ?u16 {
    if (a) |i| {
        if (b) |j| return i + j;
    }
    return null;
}

fn checkSlotsAvailability(slots: [HALLWAY_SIZE]Amphipod, a: usize, b: usize) bool {
    const i = if (a < b)
        [2]usize{ a, b }
    else
        [2]usize{ b, a };
    for (slots[i[0]..i[1] + 1]) |slot| {
        if (slot != .None) return false;
    }
    return true;
}

fn checkSlotsAvailabilityExcl(slots: [HALLWAY_SIZE]Amphipod, a: usize, b: usize) bool {
    const i = if (a < b)
        [2]usize{ a + 1, b }
    else
        [2]usize{ b, a - 1 };
    for (slots[i[0]..i[1] + 1]) |slot| {
        if (slot != .None) return false;
    }
    return true;
}

fn dist(a: u16, b: u16) u16 {
    return if (a < b) b - a else a - b;
}

fn roomPos(i: anytype) u16 {
    return 2 + 2 * @intCast(u16, i);
}

const Cache = std.AutoHashMap(u64, ?u16);

const Room = struct {
    slots: [ROOM_SIZE]Amphipod,
    size: u3,
};

const Burrow = struct {
    hallway: [HALLWAY_SIZE]Amphipod,
    rooms: [4]Room,

    fn hash(self: Burrow) u64 {
        var result: u64 = 0;
        for (HALLWAY_POINTS) |point| {
            result *= 5;
            result += @enumToInt(self.hallway[point]);
        }
        for (self.rooms) |room| {
            for (room.slots) |slot| {
                result *= 5;
                result += @enumToInt(slot);
            }
        }
        return result;
    }

    fn getEnergy(self: Burrow, cache: *Cache) std.mem.Allocator.Error!?u16 {
        const h = self.hash();
        if (cache.get(h)) |energy| {
            return energy;
        } else {
            const energy = try self.calcEnergy(cache);
            try cache.put(h, energy);
            return energy;
        }
    }

    fn isSolved(self: Burrow) bool {
        for (self.rooms) |room, i| {
            const goal = Amphipod.fromIndex(@intCast(u3, i));
            for (room.slots) |slot| {
                if (slot != goal) return false;
            }
        }
        return true;
    }

    fn calcEnergy(self: Burrow, cache: *Cache) !?u16 {
        if (self.isSolved()) {
            return 0;
        }

        return optMin(try self.upsteps(cache), try self.downsteps(cache));
    }

    fn upsteps(self: Burrow, cache: *Cache) !?u16 {
        var result: ?u16 = null;
        for (self.rooms) |room, i| {
            const goal = Amphipod.fromIndex(@intCast(u3, i));
            // check if something needs to be popped off
            for (room.slots[0..room.size]) |slot| {
                if (slot != goal) break;
            } else continue;
            const room_pos = roomPos(i);
            // if so, do it
            const base_cost = 1 + ROOM_SIZE - room.size;
            for (HALLWAY_POINTS) |pos| {
                const cost = base_cost + dist(room_pos, pos);
                if (!checkSlotsAvailability(self.hallway, pos, room_pos)) {
                    continue;
                }
                var new_burrow = self;
                const amphipod = room.slots[room.size - 1];
                new_burrow.hallway[pos] = amphipod;
                new_burrow.rooms[i].slots[room.size - 1] = .None;
                new_burrow.rooms[i].size -= 1;
                const new_energy = try new_burrow.getEnergy(cache);
                result = optMin(result, optAdd(cost * COSTS[amphipod.toIndex()], new_energy));
            }
        }
        return result;
    }

    fn downsteps(self: Burrow, cache: *Cache) !?u16 {
        var result: ?u16 = null;
        for (HALLWAY_POINTS) |pos| {
            const amphipod = self.hallway[pos];
            if (amphipod == .None) {
                continue;
            }
            const i = amphipod.toIndex();
            const room = self.rooms[i];
            for (room.slots[0..room.size]) |slot| {
                if (slot != amphipod) break;
            } else {
                const room_pos = roomPos(i);
                const base_cost = ROOM_SIZE - room.size;
                const cost = base_cost + dist(room_pos, pos);
                if (!checkSlotsAvailabilityExcl(self.hallway, pos, room_pos)) {
                    continue;
                }
                var new_burrow = self;
                new_burrow.hallway[pos] = .None;
                new_burrow.rooms[i].slots[room.size] = amphipod;
                new_burrow.rooms[i].size += 1;
                const new_energy = try new_burrow.getEnergy(cache);
                result = optMin(result, optAdd(cost * COSTS[i], new_energy));
            }
        }
        return result;
    }
};

fn readInput(filename: []const u8) !Burrow {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();
    
    var burrow: Burrow = undefined;
    std.mem.set(Amphipod, &burrow.hallway, .None);

    try file.reader().skipBytes(31, .{});

    var transposed: [ROOM_SIZE][4]Amphipod = undefined;

    for (transposed[0..2]) |*row| {
        for (row) |*slot| {
            slot.* = switch (try file.reader().readByte()) {
                'A' => .A,
                'B' => .B,
                'C' => .C,
                'D' => .D,
                else => return error.InvalidCharacter,
            };
            _ = try file.reader().readByte();
        }
        try file.reader().skipBytes(6, .{});
    }

    transposed[3] = transposed[1];

    transposed[1] = .{ .D, .C, .B, .A };
    transposed[2] = .{ .D, .B, .A, .C };

    for (burrow.rooms) |*room, i| {
        room.size = ROOM_SIZE;
        for (room.slots) |*slot, j| {
            slot.* = transposed[ROOM_SIZE - 1 - j][i];
        }
    }

    return burrow;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    var cache = Cache.init(allocator);
    defer cache.deinit();

    const burrow = try readInput("input");
    std.debug.print("{}\n", .{try burrow.getEnergy(&cache)});
}
