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

    var fld_name: []const u8 = "database.host";
    const dbhost_str = try zgf.getFieldVal([]const u8, ast, fld_name);
    std.debug.print("Field = {s}      value = {s}\n", .{ fld_name, dbhost_str });

    fld_name = "database.port";
    const dbport_u16 = try zgf.getFieldVal(u16, ast, fld_name);
    std.debug.print("Field = {s}      value = {d}\n", .{ fld_name, dbport_u16 });

    fld_name = "database.user";
    const dbhost_user = try zgf.getFieldVal([]const u8, ast, fld_name);
    std.debug.print("Field = {s}      value = {s}\n", .{ fld_name, dbhost_user });

    fld_name = "database.password";
    const dbhost_password = try zgf.getFieldVal([]const u8, ast, fld_name);
    std.debug.print("Field = {s}  value = {s}\n", .{ fld_name, dbhost_password });

    fld_name = "decimal_separator";
    const dec_separ_u8 = try zgf.getFieldVal(u8, ast, fld_name);
    std.debug.print("Field = {s}  value = {c}\n", .{ fld_name, dec_separ_u8 });

    fld_name = "months_in_year";
    const months_in_year_u8 = try zgf.getFieldVal(u8, ast, fld_name);
    std.debug.print("Field = {s}     value = {d}\n", .{ fld_name, months_in_year_u8 });

    fld_name = "newline_char";
    const newline_char_u8 = try zgf.getFieldVal(u8, ast, fld_name);
    std.debug.print("Field = {s}       value = 0x{X:0>2}\n", .{ fld_name, newline_char_u8 });

    fld_name = "pi";
    const pi_f64 = try zgf.getFieldVal(f64, ast, fld_name);
    std.debug.print("Field = {s}                 value = {d}\n", .{ fld_name, pi_f64 });

    fld_name = "hello";
    const hello_unicode_str = try zgf.getFieldVal([]const u8, ast, fld_name);
    std.debug.print("Field = {s}              value = {s}\n", .{ fld_name, hello_unicode_str });

    fld_name = "unicode_char";
    const unicode_char_u21 = try zgf.getFieldVal(u21, ast, fld_name);
    std.debug.print("Field = {s}       value = {u}\n", .{ fld_name, unicode_char_u21 });

    std.debug.print("Field = primes             value = [ ", .{});
    var buf = [_]u8 { 0 } ** 22;
    for (0..5) |i|
    {
        const buf_slice = try std.fmt.bufPrint(&buf, "primes[{d}]", .{i});
        const int_u8 = try zgf.getFieldVal(u8, ast, buf_slice);
        std.debug.print("{d}, ", .{ int_u8 });
    }
    std.debug.print("]\n", .{});

    std.debug.print("Field = factorials         value = [ ", .{});
    for (0..12) |i|
    {
        const buf_slice = try std.fmt.bufPrint(&buf, "factorials[{d}]", .{i});
        const int_u32 = try zgf.getFieldVal(u32, ast, buf_slice);
        std.debug.print("{d}, ", .{ int_u32 });
    }
    std.debug.print("]\n", .{});

    std.debug.print("Field = slc_of_structs     value = [ ", .{});
    for (0..2) |i|
    {
        const ham_buf_slice = try std.fmt.bufPrint(&buf, "slc_of_structs[{d}].ham", .{i});
        const ham_int_u32 = try zgf.getFieldVal(u32, ast, ham_buf_slice);
        const eggs_buf_slice = try std.fmt.bufPrint(&buf, "slc_of_structs[{d}].eggs", .{i});
        const eggs_int_u32 = zgf.getFieldVal(u32, ast, eggs_buf_slice) catch |err|
        {   // We actually must check these things for all fields
            std.debug.print("{{ ham = {d}, eggs = {} }}, ", .{ ham_int_u32, err });
            continue;
        };
        std.debug.print("{{ ham = {d}, eggs = {d} }}, ", .{ ham_int_u32, eggs_int_u32 });
    }
    std.debug.print("]\n", .{});

    std.debug.print("Field = slc_of_arrays      value = [ ", .{});
    for (0..2) |i|
    {
        std.debug.print("[ ", .{});
        for (0..(3 - i)) |j|
        {
            const buf_slice = try std.fmt.bufPrint(&buf, "slc_of_arrays[{d}].[{d}]", .{i, j});
            const int_u32 = try zgf.getFieldVal(u32, ast, buf_slice);
            std.debug.print("{d}, ", .{ int_u32 });
        }
        std.debug.print("], ", .{});
    }
    std.debug.print("]\n", .{});

    std.debug.print("Field = slc_of_slices      value = [ ", .{});
    for (0..3) |i|
    {
        std.debug.print("[ ", .{});
        for (0..(3 - i)) |j|
        {
            const buf_slice = try std.fmt.bufPrint(&buf, "slc_of_slices[{d}].[{d}]", .{i, j});
            const int_u32 = try zgf.getFieldVal(u32, ast, buf_slice);
            std.debug.print("{d}, ", .{ int_u32 });
        }
        std.debug.print("], ", .{});
    }
    std.debug.print("]\n", .{});
}
