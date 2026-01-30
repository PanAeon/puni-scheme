

const std = @import("std");
const NodePtr = @import("Node.zig").NodePtr;
// what is env? (#(a b c) #(1 2 3))
pub fn getEnv(env: NodePtr, _level: i48, _pos: i48) ?NodePtr {
    var level = _level;
    var current = env;
    while (level > 0 and current.getId() != .nil) {
        var p = current.cast(.pair);
        level -= 1;
        current = p.snd;
    }
    if (current.getId() == .nil) {
        return null;
    }
    var data = current.cast(.pair).fst.cast(.vector).xs;
    if (_pos < data.len) {
        return data[@intCast(_pos)];
    } else {
        std.debug.panic("incorrect pos: {d}", .{_pos});
    }
}
pub fn getEnvRef(env: NodePtr, _level: i48, _pos: i48) ?*NodePtr {
    var level = _level;
    var current = env;
    while (level > 0 and current.getId() != .nil) {
        var p = current.cast(.pair);
        level -= 1;
        current = p.snd;
    }
    if (current.getId() == .nil) {
        return null;
    }
    var data = current.cast(.pair).fst.cast(.vector).xs;
    if (_pos < data.len) {
        return &data[@intCast(_pos)];
    } else {
        std.debug.panic("incorrect pos: {d}", .{_pos});
    }
}

