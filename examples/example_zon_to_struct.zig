const std = @import("std");
const zgf = @import("zon_get_fields.zig");

pub fn main() !void
{
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocr = gpa.allocator();

    // `Ast.parse` requires a sentinel (0) terminated slice, so we pass 0 as the sentinel value (last arg)
    const zon_txt = try std.fs.cwd().readFileAllocOptions(allocr, "my.zon", std.math.maxInt(usize),
                                                          null, @alignOf(u8), 0);
    defer allocr.free(zon_txt);

    var ast = try std.zig.Ast.parse(allocr, zon_txt, .zon);
    defer ast.deinit(allocr);

    const MyStruct = struct
    {
        const DatabaseSettings = struct
        {
            host: []const u8 = &.{}, // Since this is a slice of _const_ u8,
                                     // it will be addressing the string in ast.source
            //host: []u8 = &.{}, // This version requires passing a non-null allocator
                                 // to zonToStruct, and freeing `host` when we're done
            //host: [20]u8 = [_]u8 {33} ** 20, // This version is filled with string/array elements up
                                             // to this field's capacity, or padded with 0 bytes,
                                             // if the string/array hasn't got enough elements
            port: u16 = 0,
        };
        database: DatabaseSettings = .{},
        decimal_separator: u8 = 0,
        months_in_year: u8 = 0,
        newline_char: u8 = 0,
        pi: f64 = 0.0
    };
    var ms = MyStruct {};
    try zgf.zonToStruct(&ms, ast, null); // We MUST provide `allocr` if `host` is []u8
    //defer allocr.free(ms.database.host); // Must free `host` if it's a slice of non-const u8's

    std.debug.print("Field = database.host      value = {s}\n", .{ ms.database.host });
    std.debug.print("Field = database.port      value = {d}\n", .{ ms.database.port });
    std.debug.print("Field = decimal_separator  value = {c}\n", .{ ms.decimal_separator });
    std.debug.print("Field = months_in_year     value = {d}\n", .{ ms.months_in_year });
    std.debug.print("Field = newline_char       value = 0x{X:0>2}\n", .{ ms.newline_char });
    std.debug.print("Field = pi                 value = {d}\n", .{ ms.pi });
}
