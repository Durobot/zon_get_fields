# zon_get_fields

Several functions to facilitate the process of walking Abstract Syntax Trees (ASTs) generated from [ZON](https://github.com/ziglang/zig/issues/14290) text, and fetch the values of the fields from the AST. 

But first you have to call `std.zig.Ast.parse` to tokenize and parse your ZON, creating the AST.

Tested with Zig **0.14.0-dev.1421+f87dd43c1**.

Support for older versions, like Zig **0.12.0**, **0.12.1**, **0.13.0**, and **0.14.0-dev.91+a154d8da8** was dropped because of [this breaking change in the standard library](https://github.com/ziglang/zig/commit/0fe3fd01ddc2cd49c6a2b939577d16b9d2c65ea9).

If you need a version of zon_get_fields that works with older Zig versions, [get this release](https://github.com/Durobot/zon_get_fields/releases/tag/v0.1-beta). It still probably won't work with Zig 0.11, but you're welcome to try it and report back, although I'm not too keen on backporting.

**zon_get_fields** is licensed under the [the MIT License](https://en.wikipedia.org/w/index.php?title=MIT_License&useskin=vector). You are more than welcome to drop `zon_get_fields.zig` into your project (don't forget to `const zgf = @import("zon_get_fields.zig");`), or you can use the Zig package manager:

1. In your project's `build.zig.zon`, in `.dependencies`, add

   ```zig
   .zon_get_fields =
   .{
       .url = "https://github.com/Durobot/zon_get_fields/archive/<GIT COMMIT HASH, 40 HEX DIGITS>.tar.gz",
       .hash = "<ZIG PACKAGE HASH, 68 HEX DIGITS>" // Use arbitrary hash, get correct hash from the error 
   }
   ```

2. In your project's `build.zig`, in `pub fn build`, before `b.installArtifact(exe);`, add

   ```zig
   const zgf = b.dependency("zon_get_fields",
   .{
       .target = target,
       .optimize = optimize,
   });
   exe.root_module.addImport("zon_get_fields", zgf.module("zon_get_fields"));
   ```

3. Add `const zgf = @import("zon_get_fields");`in your source file(s).

3. Build your project with `zig build`, as you normally do.

Things I like:

1. Hey, it works! At least it looks like it does, and since nothing like this exists in the standard library, I consider it a success;
2. You can throw your struct and an AST at a function (`pub fn zonToStruct`), and have your struct filled with data. The fields that have matching values in the provided AST, that is, and you get to know what was filled and what was not - look and the returned struct;
3. Or you can fetch the values of the fields you need using string paths to indicate them, calling `pub fn getFieldVal`. It's fine if all you need is a couple of fields;
3. All the basics are covered - signed and unsigned integers and floats, strings, single characters, booleans, nested structs, arrays, arrays of arrays, arrays of structs (in fact, any combination of structs and arrays should work).

Things I don't really like:

1. No AST validity verification. Thinking about ZON schemas or similar in the future, maybe?
2. If you use the second approach, `pub fn getFieldVal`, for every field you want to read you've got to specify the path. The path is split, the AST is walked, and if you have rather large structures to fetch, a lot of this work is repeated for each field. Not the best approach performance-wise. But you can switch to `pub fn zonToStruct`, which fills all the fields it can it one go;
3. An ugly hack that I had to use in order to get negative values from the fields, both integers and floating point. See `fn fulllTokenSlice`.  I can't be sure my approach works correctly in all situations, or will continue to work in the future, so I feel really uneasy about using it;

For examples of how to use them, turn to the test sections in `zon_parse.zig`:

1. Find `zonToStruct Tests` [comment](https://github.com/Durobot/zon_get_fields/blob/fcf26e7fa125fce56e411a293b285f746417bd15/src/zon_get_fields.zig#L1489) for `pub fn zonToStruct` approach - filling your struct all at once;
2. Find `getFieldVal Tests` [comment](https://github.com/Durobot/zon_get_fields/blob/fcf26e7fa125fce56e411a293b285f746417bd15/src/zon_get_fields.zig#L427) for `pub fn getFieldVal`approach - fetching field values one by one, as you provide string paths to each field.

Or check out the short examples below.

Say you've got this ZON file (`my.zon`):

```zon
.{
    .database =
    .{
        .host = "127.0.0.1",
        .port = 5432,
        .user = "superadmin",
        .password = "supersecret",
    },
    .decimal_separator = '.',
    .months_in_year = 12,
    .newline_char = '\n',
    .pi = 3.14159265359,
    .hello = "こんにちは",
    .unicode_char = '⚡',
    .primes = .{ 2, 3, 5, 7, 11 },
    .factorials = .{ 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800 },
    .slc_of_structs = .{ .{ .ham = 10, .eggs = 2 }, .{ .ham = 5 } },
    .slc_of_arrays = .{ .{ 10, 20, 30 }, .{ 40, 50, }, },
    .slc_of_slices = .{ .{ 1, 2, 3 }, .{ 4, 5 }, .{ 6 } }
}
```
Then you can either:
1. Define a struct, pass a pointer to it together with AST built from `my.zon` to pub `fn zonToStruct`, get your struct filled, and get a report struct which mirrors the fields of your struct, indicating whether they were filled or not (see [this table](#Report-struct-field-types) for report struct field types):

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
```



##### Report struct field types

| Target struct field                      | Report struct field                                          |
| ---------------------------------------- | ------------------------------------------------------------ |
| Primitive types (integers, floats, etc.) | ZonFieldResult enum.                                         |
| Array of elements of a primitive type    | Array of ZonFieldResult enum elements.                       |
| Array of arrays, structs or slices       | Array of arrays, structs, slices, or ZonFieldResult enums (for target array of slices of primitive elements).<br />The type of report array element depends on the type of target array elements. |
| Struct                                   | Struct. Field types depend on target struct field types.     |
| Slice of elements of a primitive type    | ZonFieldResult enum.                                         |
| Slice of arrays                          | Slice of arrays. Report array element type depends on the target array element type.<br />**Note**: (1) allocator must be provided (not null), since report slice has to be allocated; (2) caller owns the report struct and must deallocate this report slice. |
| Slice of structs                         | Slice of structs. Report struct field types depend on the target struct field types.<br />**Note**: (1) allocator must be provided (not null), since report slice has to be allocated; (2) caller owns the report struct and must deallocate this report slice. |
| Slice of slices                          | If target nested slices contain primitive type elements (integers, floats, etc.), then the report will contain a slice of ZonFieldResult enum elements.<br />If target nested slices contain structs, arrays or slices, then the report will contain a slice of matching structs, arrays or slices. See target struct field types 'struct', 'array' or 'slice' above.<br />**Note**: (1) allocator must be provided (not null), since report slice has to be allocated; (2) caller owns the report struct and must deallocate this report slice. |