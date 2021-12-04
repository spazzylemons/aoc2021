const std = @import("std");

const BINGO_SIZE = 5;

const BingoBoard = struct {
    spaces: [BINGO_SIZE][BINGO_SIZE]u8,
    marks: [BINGO_SIZE][BINGO_SIZE]bool,

    fn init(spaces: [BINGO_SIZE][BINGO_SIZE]u8) BingoBoard {
        return .{
            .spaces = spaces,
            .marks = std.mem.zeroes([BINGO_SIZE][BINGO_SIZE]bool),
        };
    }

    fn mark(self: *BingoBoard, number: u8) bool {
        for (self.spaces) |row, y| {
            for (row) |space, x| {
                if (space == number) {
                    if (self.marks[y][x]) return false;
                    self.marks[y][x] = true;
                    return self.check(y, x);
                }
            }
        }
        return false;
    }

    fn checkRow(self: BingoBoard, y: usize) bool {
        var x: usize = 0;
        while (x < BINGO_SIZE) : (x += 1) {
            if (!self.marks[y][x]) return false;
        }
        return true;
    }

    fn checkCol(self: BingoBoard, x: usize) bool {
        var y: usize = 0;
        while (y < BINGO_SIZE) : (y += 1) {
            if (!self.marks[y][x]) return false;
        }
        return true;
    }

    fn check(self: BingoBoard, y: usize, x: usize) bool {
        if (self.checkRow(y)) return true;
        if (self.checkCol(x)) return true;
        return false;
    }

    fn sum(self: BingoBoard) u64 {
        var result: u64 = 0;
        for (self.marks) |row, y| {
            for (row) |m, x| {
                if (!m) {
                    result += self.spaces[y][x];
                }
            }
        }
        return result;
    }
};

const BingoSubsystem = struct {
    // for drawing numbers
    numbers: []u8,
    index: usize,
    // boards in play
    boards: []BingoBoard,

    fn init(numbers: []u8, boards: []BingoBoard) BingoSubsystem {
        return .{
            .numbers = numbers,
            .index = 0,
            .boards = boards,
        };
    }

    fn next(self: *BingoSubsystem) ?u64 {
        // draw a number
        const number = self.numbers[self.index];
        self.index += 1;
        // check for a winning board
        for (self.boards) |*board| {
            if (board.mark(number)) {
                // board is a winning board
                return board.sum() * number;
            }
        }
        return null;
    }
};

fn readLine(file: std.fs.File, buf: []u8) !?[]u8 {
    return file.reader().readUntilDelimiter(buf, '\n') catch |err| switch (err) {
        error.EndOfStream => return null,
        else => |e| return e,
    };
}

fn getNumbers(allocator: *std.mem.Allocator, file: std.fs.File) ![]u8 {
    var buf: [512]u8 = undefined;
    const line = try file.reader().readUntilDelimiter(&buf, '\n');

    var numbers = try allocator.alloc(u8, std.mem.count(u8, line, ",") + 1);
    errdefer allocator.free(numbers);

    var it = std.mem.split(u8, line, ",");
    for (numbers) |*num| {
        num.* = try std.fmt.parseUnsigned(u8, it.next().?, 10);
    }

    return numbers;
}

fn boardDelimiter(file: std.fs.File) !bool {
    _ = file.reader().readByte() catch |err| switch (err) {
        error.EndOfStream => return false,
        else => |e| return e,
    };
    return true;
}

fn getBoard(file: std.fs.File) !BingoBoard {
    var buf: [64]u8 = undefined;
    var spaces: [BINGO_SIZE][BINGO_SIZE]u8 = undefined;
    for (spaces) |*row| {
        const line = try file.reader().readUntilDelimiter(&buf, '\n');
        var i: usize = 0;
        for (row) |*space| {
            while (i < line.len and line[i] == ' ') i += 1;
            var j = i;
            while (j < line.len and line[j] != ' ') j += 1;
            space.* = try std.fmt.parseUnsigned(u8, line[i..j], 10);
            i = j;
        }
    }
    return BingoBoard.init(spaces);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    const file = try std.fs.cwd().openFile("input", .{});
    defer file.close();

    const numbers = try getNumbers(allocator, file);
    defer allocator.free(numbers);

    var boards = std.ArrayListUnmanaged(BingoBoard) {};
    defer boards.deinit(allocator);

    while (try boardDelimiter(file)) {
        try boards.append(allocator, try getBoard(file));
    }

    var bingo = BingoSubsystem.init(numbers, boards.items);

    while (true) {
        if (bingo.next()) |sum| {
            std.debug.print("{}\n", .{sum});
            return;
        }
    }
}
