const std = @import("std");

const Node = @import("Node.zig").Node;
// simple memory pool for cons cells
const ConsAllocator = @This();

// const PageSize = 1_048_576; // 1 mb
const PageSize = 8388608; // 8 mb
// const NumNodes = PageSize / @sizeOf(Page);
// so far taken from https://github.com/Vexu/toy-lang/blob/master/src/Gc.zig
pub const Page = struct {
    comptime {
        // 2^20, 1 MiB
        std.debug.assert(@sizeOf(Page) == PageSize);
    }
    const List = std.ArrayListUnmanaged(*Page);

    const State = enum {
        empty,
        black,
    };

    const NumItems = @divFloor(PageSize - @sizeOf(u32) * 2, (@sizeOf(Node.Pair) + @sizeOf(State)));
    const PaddingSize = PageSize - @sizeOf(u32) * 2 - ( NumItems * (@sizeOf(Node.Pair) + @sizeOf(State)) );

    data: [NumItems]Node.Pair,
    state: [NumItems]State,
    __padding: [PaddingSize]u8,

    free: u32,
    marked: u32,

    pub fn create() !*Page {
        const page = try std.heap.page_allocator.create(Page);
        @memset(std.mem.bytesAsSlice(usize, std.mem.asBytes(page)), 0);
        return page;
    }

    pub fn destroy(self: *Page) void {
        std.heap.page_allocator.destroy(self);
    }

    pub fn alloc(self: *Page) ?*Node.Pair {
        while (self.free < self.data.len) {
            defer self.free += 1;
            if (self.state[self.free] == .empty) {
                self.state[self.free] = .black;
                return &self.data[self.free];
            }
        }
        return null;
    }

    pub fn dealloc(self: *Page, p: *const Node.Pair) void {
        if (self.indexOf(p)) |i| {
            self.state[i] = .empty;
            if (i < self.free) {
                self.free = i;
            }
        } else {
            @panic("pair not found");
        }
    }

    pub fn gc(self: *Page) void {
        var i: u32 = NumItems;
        while (i > 0) {
            i-=1;
            if (!self.data[i].base.marked) {
                self.state[i] = .empty;
                self.free = i;
            }

        }
    }
    fn indexOf(page: *Page, value: *const Node.Pair) ?u32 {
        // is the value before this page
        if (@intFromPtr(value) < @intFromPtr(&page.data[0])) return null;
        // is the value after this page
        if (@intFromPtr(value) > @intFromPtr(&page.data[page.data.len - 1])) return null;

        // value is in this page
        return @intCast((@intFromPtr(value) - @intFromPtr(&page.data[0])) / @sizeOf(Node.Pair));
    }


};
