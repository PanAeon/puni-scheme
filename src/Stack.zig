const std = @import("std");

const StackError = error{
    StackOverflow,
    StackUnderflow,
};
pub fn Stack(comptime T: type) type {
    return struct {
        items: []T,
        size: usize = 0,
        const Self = @This();

        pub fn init(allocator: std.mem.Allocator, n: usize) !Self {
            return Self {
                .items = try allocator.alloc(T, n)
            };
        }
        pub fn deinit(self: Self, allocator: std.mem.Allocator) void {
            allocator.free(self.items);
        }
        pub fn dupe(self: *Self) !void {
            try self.push(try self.head());
        }
        pub fn push(self: *Self, value: T) !void {
            if (self.size >= self.items.len) {
                return error.StackOverflow;
            }
            self.items[self.size] = value;
            self.size += 1;
        }
        pub fn pop(self: *Self) !T {
            if (self.size == 0) {
                return error.StackUnderflow;
            }
            self.size -= 1;
            return self.items[self.size];
        }
        pub fn drop1(self: *Self) !void {
            if (self.size == 0) {
                return error.StackUnderflow;
            }
            self.size -= 1;
        }
        pub fn dropN(self: *Self, n:usize) !void {
            if (self.size < n) {
                return error.StackUnderflow;
            }
            self.size -= n;
        }
        pub fn popOrDie(self: *Self) void {
            if (self.size == 0) {
                @panic("Stack underflow");
            }
            self.size -= 1;
        }
        pub fn head(self: *Self) !T {
            if (self.size == 0) {
                return error.StackUnderflow;
            }
            return self.items[self.size - 1];
        }
        pub fn setHead(self: *Self, x: *const T) !void {
            if (self.size == 0) {
                return error.StackUnderflow;
            }
            self.items[self.size - 1] = x;
        }
        pub fn nth(self: *Self, n:usize) !T {
            if (self.size < n) {
                return error.StackUnderflow;
            }
            return self.items[self.size - 1 - n];
        }
        pub fn shift1(self: *Self, n:usize) !void {
            if (n < 2) {
                return;
            }
            if (self.size < n) {
                return error.StackUnderflow;
            }
            const firstIdx = self.size - n;
            const first = self.items[firstIdx];
            for (firstIdx..self.size-1) |i| {
                self.items[i] = self.items[i+1];
            }
            self.items[self.size - 1] = first;

            // return self.items[self.size - 1 - n];
        }
        pub fn shift(self: *Self, n:u32, m:u32) !void {
            if (m == 0) {
                return;
            }
            const firstIdx = self.size - n - m;
            for (firstIdx..self.size-m) |i| {
                self.items[i] = self.items[i+m];
            }
            self.size -= m;
        }
        pub fn headRef(self: *Self) !*T {
            if (self.size == 0) {
                return error.StackUnderflow;
            }
            return &self.items[self.size - 1];
        }
        pub fn clear(self: *Self) void {
            self.size = 0;
        }
        pub fn reverseInPlace(self: *Self, n: usize) !void {
            if (self.size < n) {
                return error.StackUnderflow;
            }
            if (n < 2) {
                return;
            }
            var tmp: T = undefined; 
            for (0..(n/2)) |i| {
                tmp = self.items[self.size - 1 - i];
                self.items[self.size - 1 - i] = self.items[self.size - n + i];
                self.items[self.size - n + i] = tmp;
            }
        }
    };
}
