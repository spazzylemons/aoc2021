const std = @import("std");

/// Snailfish numbers are full binary trees. We store then using an array, with
/// branch notes being elements where two more nodes follow, and leaf notes being
/// one element containing their value. Functions traversing trees takee and return
/// an index into the array where the target node is.
const Node = union(enum) { Branch, Leaf: u8 };

/// The array of nodes.
const Tree = std.ArrayList(Node);

/// Attempt to add a value to the node, if it is a leaf node. Returns true on success.
fn tryAddLeaf(node: *Node, value: u8) bool {
    switch (node.*) {
        .Leaf => |*leaf| {
            leaf.* += value;
            return true;
        },
        .Branch => return false,
    }
}

/// Add the given leaf value to the first leaf before it, if any.
fn addPrev(tree: *Tree, index: usize, leaf: u8) void {
    var prev = index;
    while (!tryAddLeaf(&tree.items[prev], leaf)) {
        if (prev == 0) break;
        prev -= 1;
    }
}

/// Add the given leaf value to the next leaf after it, if any.
fn addNext(tree: *Tree, index: usize, leaf: u8) void {
    var next = index;
    while (!tryAddLeaf(&tree.items[next], leaf)) {
        next += 1;
        if (next == tree.items.len) break;
    }
}

/// Get and remove two leaf values from the tree.
fn removeExplodedTerms(tree: *Tree, index: usize) [2]u8 {
    const l = tree.items[index].Leaf;
    const r = tree.items[index + 1].Leaf;

    // there is no orderedRemove for multiple elements, so we remove two at a time ourselves.
    const new_len = tree.items.len - 2;
    for (tree.items[index..new_len]) |*b, j| {
        b.* = tree.items[index + 2 + j];
    }
    tree.items.len = new_len;

    return .{ l, r };
}

fn explode(tree: *Tree, index: usize, depth: usize) ?usize {
    if (tree.items[index] == .Branch) {
        if (depth == 4) {
            // if depth is four, an explosion occurs
            const pair = removeExplodedTerms(tree, index + 1);
            // add previous and next
            addPrev(tree, index, pair[0]);
            addNext(tree, index, pair[1]);
            // replace branch with leaf with value 0
            tree.items[index] = .{ .Leaf = 0 };
            // done with explode operation
            return null;
        } else {
            // depth is not yet four, keep looking
            if (explode(tree, index + 1, depth + 1)) |index1| {
                return explode(tree, index1, depth + 1);
            }
            return null;
        }
    } else {
        // skip over leaves
        return index + 1;
    }
}

fn split(tree: *Tree, index: usize) std.mem.Allocator.Error!?usize {
    switch (tree.items[index]) {
        .Branch => {
            // try to split left and right halves
            if (try split(tree, index + 1)) |index1| {
                return try split(tree, index1);
            }
            return null;
        },
        .Leaf => |leaf| {
            if (leaf >= 10) {
                // leaf is big enough to split, so we create a branch in its place
                tree.items[index] = .Branch;
                // and add the split values in the branch
                const new_leaves = [2]Node{
                    .{ .Leaf = leaf / 2 },
                    .{ .Leaf = (leaf + 1) / 2 },
                };
                try tree.insertSlice(index + 1, &new_leaves);
                // dont with split operation
                return null;
            }
            return index + 1;
        }
    }
}

fn reduce(tree: *Tree) !void {
    while (true) {
        // attempt an explode operation
        if (explode(tree, 0, 0) == null) {
            continue;
        }
        // attepmt a split operation
        if ((try split(tree, 0)) == null) {
            continue;
        }
        // tree is fully reduced
        break;
    }
}

const Magnitude = struct { sum: u16, index: usize };

fn magnitude(tree: *Tree, index: usize) Magnitude {
    switch (tree.items[index]) {
        .Leaf => |leaf| {
            return .{ .sum = leaf, .index = index + 1 };
        },
        .Branch => {
            const l = magnitude(tree, index + 1);
            const r = magnitude(tree, l.index);
            return .{ .sum = 3 * l.sum + 2 * r.sum, .index = r.index };
        },
    }
}

fn assembleTree(tree: *Tree, file: std.fs.File) !void {
    while (true) {
        switch (try file.reader().readByte()) {
            '[' => try tree.append(.Branch),
            '0'...'9' => |n| try tree.append(.{ .Leaf = n - '0' }),
            '\n' => return,
            else => {},
        }
    }
}

fn readTrees(allocator: *std.mem.Allocator, trees: *std.ArrayList([]Node), filename: []const u8) !void {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    // read all trees from input
    while (true) {
        var tree = Tree.init(allocator);
        defer tree.deinit();

        assembleTree(&tree, file) catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };

        const new_tree = try trees.addOne();
        new_tree.* = &.{};
        new_tree.* = tree.toOwnedSlice();
    }
}

fn partOne(allocator: *std.mem.Allocator, trees: []const []const Node) !u16 {
    var tree = Tree.init(allocator);
    defer tree.deinit();

    try tree.appendSlice(trees[0]);

    for (trees[1..]) |slice| {
        try tree.insert(0, .Branch);
        try tree.appendSlice(slice);
        try reduce(&tree);
    }

    return magnitude(&tree, 0).sum;
}

fn partTwo(allocator: *std.mem.Allocator, trees: []const []const Node) !u16 {
    var max_magnitude: u16 = 0;

    for (trees) |l| {
        for (trees) |r| {
            if (l.ptr == r.ptr) continue;

            var tree = try Tree.initCapacity(allocator, 1 + l.len + r.len);
            defer tree.deinit();

            tree.appendAssumeCapacity(.Branch);
            tree.appendSliceAssumeCapacity(l);
            tree.appendSliceAssumeCapacity(r);

            try reduce(&tree);

            const mag = magnitude(&tree, 0).sum;

            if (mag > max_magnitude) {
                max_magnitude = mag;
            }
        }
    }

    return max_magnitude;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    const stdout = std.io.getStdOut().writer();

    var trees = std.ArrayList([]Node).init(allocator);
    defer {
        for (trees.items) |tree| {
            allocator.free(tree);
        }
        trees.deinit();
    }

    try readTrees(allocator, &trees, "input");

    try stdout.print("part one: {}\n", .{try partOne(allocator, trees.items)});
    try stdout.print("part two: {}\n", .{try partTwo(allocator, trees.items)});
}
