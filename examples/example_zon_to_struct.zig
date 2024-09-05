const std = @import("std");
const zgf = @import("zon_get_fields");

pub fn main() !void
{
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocr = gpa.allocator();

    // `Ast.parse` requires a sentinel (0) terminated slice, so we pass 0 as the sentinel value (last arg)
    const zon_txt = try std.fs.cwd().readFileAllocOptions(allocr, "examples/my.zon", std.math.maxInt(usize),
                                                          null, @alignOf(u8), 0);
    defer allocr.free(zon_txt);

    var ast = try std.zig.Ast.parse(allocr, zon_txt, .zon);
    defer ast.deinit(allocr);

    const MyStruct = struct // Field order is not important
    {
        const DatabaseSettings = struct
        {
            host: []const u8 = &.{}, // Since this is a slice of _const_ u8,
                                     // it will be addressing the string in ast.source
            port: u16 = 0,
            user: []u8 = &.{}, // Having a slice field requires passing a non-null allocator
                               // to zonToStruct, and freeing `user` when we're done
            password: [20]u8 = [_]u8 {33} ** 20, // This array is filled with string/array elements up
                                                 // to this field's capacity, or padded with 0 bytes,
                                                 // if ZON string/array hasn't got enough elements
        };
        const MyStruct = struct { ham: u32 = 0, eggs: u32 = 0 };
        database: DatabaseSettings = .{},
        decimal_separator: u8 = 0,
        months_in_year: u8 = 0,
        newline_char: u8 = 0,
        pi: f64 = 0.0,
        hello: []const u8 = &.{}, // Same as hello: []const u8 = undefined; hello.len = 0;
        unicode_char: u21 = 0,
        primes: [10]u8 = [_]u8 { 0 } ** 10,
        factorials: [10]u32 = [_]u32 { 0 } ** 10,
        slc_of_structs: []MyStruct = &.{},
        slc_of_arrays: [][3]u32 = &.{},
        slc_of_slices: [][]u32 = &.{},
    };
    var ms = MyStruct {};
    // We MUST provide `allocr` if there are slices in ms, otherwise a null is fine.
    const report = try zgf.zonToStruct(&ms, ast, allocr);
    defer allocr.free(ms.database.user); // Must free `user` since it's a slice of non-const u8's.
    defer allocr.free(ms.slc_of_structs);
    defer allocr.free(report.slc_of_structs); // Must also free the slice of structs in `report`
    defer allocr.free(ms.slc_of_arrays);
    defer allocr.free(report.slc_of_arrays);
    defer
    {
        for (ms.slc_of_slices) |s| allocr.free(s); // Deallocate nested slices first
        allocr.free(ms.slc_of_slices); // THEN deallocate the outer slice
        allocr.free(report.slc_of_slices); // `report` contains a slice of `ZonFieldResult` enums
    }

    std.debug.print("Field = database.host      value = {s}\n", .{ ms.database.host });
    std.debug.print("Field = database.port      value = {d}\n", .{ ms.database.port });
    std.debug.print("Field = database.user      value = {s}\n", .{ ms.database.user });
    std.debug.print("Field = database.password  value = {s}\n", .{ ms.database.password });
    std.debug.print("Field = decimal_separator  value = {c}\n", .{ ms.decimal_separator });
    std.debug.print("Field = months_in_year     value = {d}\n", .{ ms.months_in_year });
    std.debug.print("Field = newline_char       value = 0x{X:0>2}\n", .{ ms.newline_char });
    std.debug.print("Field = pi                 value = {d}\n", .{ ms.pi });
    std.debug.print("Field = hello              value = {s}\n", .{ ms.hello });
    std.debug.print("Field = unicode_char       value = {u}\n", .{ ms.unicode_char });
    std.debug.print("Field = primes             value = [ ", .{});
    for (ms.primes) |p| std.debug.print("{}, ", .{ p });
    std.debug.print("]\n", .{});
    std.debug.print("Field = factorials         value = [ ", .{});
    for (ms.factorials) |f| std.debug.print("{}, ", .{ f });
    std.debug.print("]\n", .{});
    std.debug.print("Field = slc_of_structs     value = [ ", .{});
    for (ms.slc_of_structs) |s| std.debug.print("{{ ham = {}, eggs = {} }}, ", .{ s.ham, s.eggs });
    std.debug.print("]\n", .{});
    std.debug.print("Field = slc_of_arrays      value = [ ", .{});
    for (ms.slc_of_arrays) |a|
    {
        std.debug.print("[ ", .{});
        for (a) |e| std.debug.print("{}, ", .{ e });
        std.debug.print("], ", .{});
    }
    std.debug.print("]\n", .{});
    std.debug.print("Field = slc_of_slices      value = [ ", .{});
    for (ms.slc_of_slices) |s|
    {
        std.debug.print("[ ", .{});
        for (s) |e| std.debug.print("{}, ", .{ e });
        std.debug.print("], ", .{});
    }
    std.debug.print("]\n\n", .{});

    // `report` describes the state of corresponding fields in `ms`
    std.debug.print("Report =\n{s}\n", .{ std.json.fmt(report, .{ .whitespace = .indent_4 }) });
}
