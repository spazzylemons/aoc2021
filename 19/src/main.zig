const std = @import("std");
const zlm = @import("zlm");

const Vec = zlm.SpecializeOn(i16).Vec3;
const Mtx = zlm.SpecializeOn(i16).Mat4;

/// The 24 possible rotation matrices.
const rot_mtxs = blk: {
    @setEvalBranchQuota(20_000);

    // sine for each quarter rotation
    const s = [4]i16{0, 1, 0, -1};
    // cosine for each quarter rotation
    const c = [4]i16{1, 0, -1, 0};
    
    // get single-axis rotation matrices
    var rxs: [4]Mtx = undefined;
    for (rxs) |*m, i| {
        m.fields = .{
            .{    1,     0,     0,     0},
            .{    0,  c[i],  s[i],     0},
            .{    0, -s[i],  c[i],     0},
            .{    0,     0,     0,     1},
        };
    }
    var rys: [4]Mtx = undefined;
    for (rys) |*m, i| {
        m.fields = .{
            .{ c[i],     0, -s[i],     0},
            .{    0,     1,     0,     0},
            .{ s[i],     0,  c[i],     0},
            .{    0,     0,     0,     1},
        };
    }
    var rzs: [4]Mtx = undefined;
    for (rzs) |*m, i| {
        m.fields = .{
            .{ c[i],  s[i],     0,     0},
            .{-s[i],  c[i],     0,     0},
            .{    0,     0,     1,     0},
            .{    0,     0,     0,     1},
        };
    }
    // select the 24 unique matrices
    var mtxs: [24]Mtx = undefined;
    var i = 0;
    for (rxs) |rx| {
        for (rys) |ry| {
            const rxy = rx.mul(ry);
            for (rzs) |rz| {
                const rxyz = rxy.mul(rz);
                for (mtxs[0..i]) |mtx| {
                    if (std.meta.eql(rxyz, mtx)) break;
                } else {
                    mtxs[i] = rxyz;
                    i += 1;
                }
            }
        }
    }
    break :blk mtxs;
};

/// Tansform a vector by a matrix.
fn transform(v: Vec, m: Mtx) Vec {
    return v.swizzle("xyz1").transform(m).swizzle("xyz");
}

/// A scanner, which can detect nearby beacons.
const Scanner = struct {
    /// The nearby beacons, relative to the scanner.
    beacons: []Vec,
    /// The transformation matrix from the scanner's relative space to absolute space.
    transform: Mtx,

    /// Get an iterator over the scanner's beacons' absolute positions.
    fn getAbsoluteBeacons(scanner: Scanner) AbsoluteBeaconIterator {
        return .{
            .beacons = scanner.beacons,
            .transform = scanner.transform,
        };
    }

    /// Get the scanner's absolute position.
    fn getPosition(scanner: Scanner) Vec {
        // scanner is at relative origin
        return transform(Vec.zero, scanner.transform);
    }

    /// An iterator over a scanner's beacons' absolute positions.
    const AbsoluteBeaconIterator = struct {
        /// The remaining beacons.
        beacons: []Vec,
        /// The transform matrix of the scanner.
        transform: Mtx,

        /// Get the next absolute position.
        fn next(it: *AbsoluteBeaconIterator) ?Vec {
            // check if any left
            if (it.beacons.len == 0) {
                return null;
            }
            // take one
            const beacon = it.beacons[0];
            it.beacons = it.beacons[1..];
            // transform it
            return transform(beacon, it.transform);
        }
    };
};

/// Read a beacon from the input.
fn readBeacon(file: std.fs.File, remaining: *bool) !?Vec {
    // set remaining as true until otherwise determined to not be true
    remaining.* = true;
    // vector components
    var components: [3]i16 = undefined;
    // current component
    var current: i16 = 0;
    var negative = false;
    var index: u8 = 0;
    // read line
    while (true) {
        const byte = file.reader().readByte() catch |err| switch (err) {
            error.EndOfStream => {
                remaining.* = false;
                return null;
            },
            else => |e| return e,
        };
        switch (byte) {
            // negative sign
            '-' => negative = true,
            // digit
            '0'...'9' => |digit| {
                current *= 10;
                current += digit - '0';
            },
            // end of current component
            ',' => {
                // store component
                if (negative) current = -current;
                components[index] = current;
                // move to next component
                index += 1;
                current = 0;
                negative = false;
            },
            // end of beacon
            '\n' => {
                // check if empty
                if (index < 2) {
                    return null;
                }
                // store component
                if (negative) current = -current;
                components[index] = current;
                // return components as vector
                return @bitCast(Vec, components);
            },
            else => unreachable,
        }
    }
}

/// Read a scanner from the input.
fn readScanner(allocator: *std.mem.Allocator, file: std.fs.File, remaining: *bool) ![]Vec {
    var beacons = std.ArrayListUnmanaged(Vec) {};
    defer beacons.deinit(allocator);

    while (try readBeacon(file, remaining)) |beacon| {
        try beacons.append(allocator, beacon);
    }

    return beacons.toOwnedSlice(allocator);
}

/// Check if there is sufficient overlap between beacons for scanners to be relative.
fn checkOverlaps(a: []Vec, b: []Vec) bool {
    var count: usize = 0;
    // naive check, but fast enough bleh
    for (a) |p| {
        for (b) |q| {
            if (p.eql(q)) {
                count += 1;
                if (count == 12) return true;
                break;
            }
        }
    }
    return false;
}

const PendingArray = std.ArrayListUnmanaged([]Vec);
const ScannerArray = std.ArrayListUnmanaged(Scanner);

/// Copy and offset an array of vectors.
fn createOffsetBeacons(p: []Vec, q: []const Vec, a: Vec) void {
    for (p) |*v, i| v.* = q[i].sub(a);
}

/// Try to add a scanner to the collection of located scanners.
fn locateScanner(allocator: *std.mem.Allocator, scanners: *ScannerArray, new_beacons: []Vec) !bool {
    // buffer to store rotated beacons of the new beacon array
    const rotated = try allocator.alloc(Vec, new_beacons.len);
    defer allocator.free(rotated);
    // buffer to store anchored positions of the new beacon array
    const r2 = try allocator.alloc(Vec, new_beacons.len);
    defer allocator.free(r2);
    // check every existing scanner for sufficient overlap
    for (scanners.items) |scanner| {
        // buffer to store anchored positions of the existing beacon array
        const r1 = try allocator.alloc(Vec, scanner.beacons.len);
        defer allocator.free(r1);
        // go through each beacon for the existing scanner
        for (scanner.beacons) |a1| {
            // anchor the points by the chosen beacon
            createOffsetBeacons(r1, scanner.beacons, a1);
            // try each rotation for the new beacons
            for (rot_mtxs) |rotation| {
                // rotate beacons
                for (rotated) |*r, i| {
                    r.* = transform(new_beacons[i], rotation);
                }
                // go through each beacon in the rotated beacons
                for (rotated) |a2| {
                    // anchor the points by the chosen beacon
                    createOffsetBeacons(r2, rotated, a2);
                    // is overlap sufficient?
                    if (checkOverlaps(r1, r2)) {
                        // add scanner with correct transform matrix
                        try scanners.append(allocator, .{
                            .beacons = new_beacons,
                            .transform = rotation
                                .mul(Mtx.createTranslation(a1.sub(a2)))
                                .mul(scanner.transform),
                        });
                        // we're done
                        return true;
                    }
                }
            }
        }
    }
    // no match
    return false;
}

/// Solve part one.
fn partOne(allocator: *std.mem.Allocator, scanners: []Scanner) !usize {
    var beacons = std.AutoHashMapUnmanaged(Vec, void) {};
    defer beacons.deinit(allocator);

    for (scanners) |scanner| {
        var it = scanner.getAbsoluteBeacons();
        while (it.next()) |beacon| {
            try beacons.put(allocator, beacon, {});
        }
    }

    return beacons.size;
}

/// Find the manhattan distance between two points.
fn manhattanDistance(v1: Vec, v2: Vec) u16 {
    var dist: u16 = 0;
    inline for (.{"x", "y", "z"}) |field| {
        const f1 = @field(v1, field);
        const f2 = @field(v2, field);
        dist += @intCast(u16, if (f1 < f2) f2 - f1 else f1 - f2);
    }
    return dist;
}

/// Solve part two.
fn partTwo(scanners: []Scanner) u16 {
    var max_dist: u16 = 0;
    for (scanners) |s1| {
        const p1 = s1.getPosition();
        for (scanners) |s2| {
            const p2 = s2.getPosition();
            const dist = manhattanDistance(p1, p2);
            if (dist > max_dist) {
                max_dist = dist;
            }
        }
    }
    return max_dist;
}

/// Load pending scanners from the input file.
fn loadScanners(allocator: *std.mem.Allocator, pending: *PendingArray) !void {
    const file = try std.fs.cwd().openFile("input", .{});
    defer file.close();

    var remaining = true;
    while (remaining) {
        try file.reader().skipUntilDelimiterOrEof('\n');
        // reaq another scanner
        const beacons = try readScanner(allocator, file, &remaining);
        errdefer allocator.free(beacons);
        try pending.append(allocator, beacons);
    }
}

/// Locate all scanners.
fn locateScanners(allocator: *std.mem.Allocator, scanners: *ScannerArray, pending: *PendingArray) !void {
    // first scanner is used as reference, considered to be at (0, 0)
    const root_scanner = try scanners.addOne(allocator);
    root_scanner.* = .{
        .beacons = pending.swapRemove(0),
        .transform = Mtx.identity,
    };
    // while there are scanners left...
    while (pending.items.len > 0) {
        // check each pending scanner
        for (pending.items) |p, i| {
            if (try locateScanner(allocator, scanners, p)) {
                _ = pending.swapRemove(i);
                break;
            }
        } else {
            @panic("cannot locate a scanner; insufficient overlap");
        }
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    var scanners = ScannerArray {};
    defer {
        for (scanners.items) |scanner| allocator.free(scanner.beacons);
        scanners.deinit(allocator);
    }
    var pending = PendingArray {};
    defer {
        for (pending.items) |p| allocator.free(p);
        pending.deinit(allocator);
    }

    try loadScanners(allocator, &pending);
    try locateScanners(allocator, &scanners, &pending);

    std.debug.print("part one: {}\n", .{try partOne(allocator, scanners.items)});
    std.debug.print("part two: {}\n", .{partTwo(scanners.items)});
}
