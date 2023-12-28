# zon_get_fields

Several functions to facilitate the process of walking Abstract Syntax Trees (ASTs) generated from [ZON](https://github.com/ziglang/zig/issues/14290) text, and fetch the values of the fields from the AST.

But first you have to call `std.zig.Ast.parse` to tokenize and parse your ZON, creating the AST.

**zon_get_fields** is licensed under the [the MIT License](https://en.wikipedia.org/w/index.php?title=MIT_License&useskin=vector). You are more than welcome to drop `zon_get_fields.zig` into your project,

Things I like:

1. Hey, it works! At looks it looks like it does, and since nothing like this exists in the standard library, I consider it a success;
2. Using string paths to indicate fields is rather convenient, I think. At least if all you need is a couple of fields;
3. All the basics are covered - signed and unsigned integers and floats, strings, single characters, booleans, nested structs, arrays, arrays of arrays, arrays of structs (in fact, any combination of structs and arrays should work).

Things I don't really like:

1. For every field you want to read you've got to specify the path. The path is split, the AST is walked, and if you have rather large structures to fetch, a lot of this work is repeated for each field. Not the best approach performance-wise;
2. An ugly hack that I had to use in order to get negative values from the fields, both integers and floating point. See `fn fulllTokenSlice`.  I can't be sure my approach works correctly in all situations, or will continue to work in the future, so I feel really uneasy about using it;
3. I had to declare my path iterator (`path_itr`) as `const` in `fn getFieldValStr`, and then use `@constCast` to circumvent this. It was either that, or make `path_itr` a `var` and throw in `_ = &path_itr;` to tell the compiler it's actually mutated - just passing a pointer to `path_itr` to `walkAst` is not enough :(

For examples of how to use them, turn to the test section at the end of `zon_parse.zig`, or check out this little program:

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
    const dbhost_str = try zgf.getFieldValStr(ast, fld_name);
    std.debug.print("Field = {s}, val_str = {s}\n", .{ fld_name, dbhost_str });

    fld_name = "database.port";
    const dbport_u16 = try zgf.getFieldValInt(u16, ast, fld_name);
    std.debug.print("Field = {s}, val_u16 = {d}\n", .{ fld_name, dbport_u16 });
}
```

Where `my.zon` is:
```zon
.{
    .database =
    .{
        .host = "127.0.0.1",
        .port = 5432,
    }
}
```