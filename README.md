# zon_get_fields

Several functions to facilitate the process of walking Abstract Syntax Trees (ASTs) generated from [ZON](https://github.com/ziglang/zig/issues/14290) text, and fetch the values of the fields from the AST.

But first you have to call `std.zig.Ast.parse` to tokenize and parse your ZON, creating the AST.

**zon_get_fields** is licensed under the [the MIT License](https://en.wikipedia.org/w/index.php?title=MIT_License&useskin=vector). You are more than welcome to drop `zon_get_fields.zig` into your project,

Things I like:

1. Hey, it works! At looks it looks like it does, and since nothing like this exists in the standard library, I consider it a success;
2. You can throw your struct and an AST at a function (`pub fn zonToStruct`), and have your struct filled with data. The fields that have matching values in the provided AST, that is;
3. Or you can fetch the values of the fields you need using string paths to indicate them, calling `pub fn getFieldVal`. It's fine if all you need is a couple of fields;
3. All the basics are covered - signed and unsigned integers and floats, strings, single characters, booleans, nested structs, arrays, arrays of arrays, arrays of structs (in fact, any combination of structs and arrays should work).

Things I don't really like:

1. The first approach,  `pub fn zonToStruct`, does not report the fields it could not fill if the provided AST contained no matching nodes. I do have some ideas on how to fix this, but nothing is implemented yet;
2. No AST validity verification. Thinking about ZON schemas or similar in the future, maybe?
3. If you use the second approach, `pub fn getFieldVal`, for every field you want to read you've got to specify the path. The path is split, the AST is walked, and if you have rather large structures to fetch, a lot of this work is repeated for each field. Not the best approach performance-wise. But you can switch to `pub fn zonToStruct`, which fills all the fields it can it one go;
4. An ugly hack that I had to use in order to get negative values from the fields, both integers and floating point. See `fn fulllTokenSlice`.  I can't be sure my approach works correctly in all situations, or will continue to work in the future, so I feel really uneasy about using it;

For examples of how to use them, turn to the test sections in `zon_parse.zig`:

1. Find `zonToStruct Tests` comment (line 1077) for `pub fn zonToStruct` approach - filling your struct all at once;
2. Find `getFieldVal Tests` comment (line 424) for `pub fn getFieldVal`approach - fetching field values one by one, as you provide string paths to each field.

Or check out the short examples below.

Say you've got this ZON file (`my.zon`):

```zon
.{
    .database =
    .{
        .host = "127.0.0.1",
        .port = 5432,
    },
    .decimal_separator = '.',
    .months_in_year = 12,
    .newline_char = '\n',
    .pi = 3.14159265359,
    .hello = "こんにちは",
    .unicode_char = '⚡'
}
```

1. Then you can either define a struct, create an instance of it and fill it with the values from `my.zon`, using pub `fn zonToStruct`:

```zig
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

    const MyStruct = struct // Field order is not important
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
        pi: f64 = 0.0,
        hello: []const u8 = &.{}, // Same as hello: []const u8 = undefined; hello.len = 0;
        unicode_char: u21 = 0
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
    std.debug.print("Field = hello              value = {s}\n", .{ ms.hello });
    std.debug.print("Field = unicode_char       value = {u}\n", .{ ms.unicode_char });
}
```

2. Or you can fetch the values one by one using `pub fn getFieldVal`:

```zig
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

    fld_name = "hello";
    const hello_unicode_str = try zgf.getFieldVal([]const u8, ast, fld_name);
    std.debug.print("Field = {s}              value = {s}\n", .{ fld_name, hello_unicode_str });

    fld_name = "unicode_char";
    const unicode_char_u21 = try zgf.getFieldVal(u21, ast, fld_name);
    std.debug.print("Field = {s}       value = {u}\n", .{ fld_name, unicode_char_u21 });
}
```