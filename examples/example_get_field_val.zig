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

    var fld_name: []const u8 = "database.host";
    const dbhost_str = try zgf.getFieldVal([]const u8, ast, fld_name);
    std.debug.print("Field = {s}      value = {s}\n", .{ fld_name, dbhost_str });

    fld_name = "database.port";
    const dbport_u16 = try zgf.getFieldVal(u16, ast, fld_name);
    std.debug.print("Field = {s}      value = {d}\n", .{ fld_name, dbport_u16 });

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
}
