// MIT License
//
// Copyright (c) 2023, 2024 Alexei Kireev
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// zon_get_fields is available from https://github.com/Durobot/zon_get_fields

const std = @import("std");

/// Limit on the length of the path, used by
/// `getFieldVal*` functions. These functions
/// call `walkAst`, which is a recursive function,
/// and `zon_fld_path_len_limit` is the recursion
/// depth limit.
pub const zon_fld_path_len_limit = 20;

pub const ZonGetFieldsError = error
{
    // errors common for `fn getFieldVal` and `fn zonToStruct`:
    PathElementNotStruct,    // One of the path elements (other than the last one) is not a sub-struct in ZON (AST).
    PathElementNotArray,     // One of the path elements (with [index]) is not an array in ZON (AST).
    BadBooleanValue,         // Field value could not be interpreted as a boolean, neither `false` nor `true`.
    BadCharValue,            // Field length is not 3, or field does not start or end with a quotation mark.
    // `fn getFieldVal`-specific errors:
    PathLimitReached,        // Field path contains too many elements (separated by dots), see `zon_fld_path_len_limit`.
    BadSeparatorPosition,    // Zero-length (empty) field path, separator (dot) at the beginning of the field path,
                             // at the end of the field path, or two consecutive dots in the path.
    BadArrIdxSyntax,         // Bad array index syntax in a field path element, e.g. one brace is missing.
    BadArrIdxValue,          // Bad array index syntax in a field path element, e.g. non-numeric character(s)
                             // in index, or the array doesn't contain enough elements.
    NotFound,                // Field not found at the provided path.
    // `fn zonToStruct`-specific errors:
    NoAllocator,             // Target struct contains field(s) that require memory allocation,
                             // but no allocator was provided (the optional allocator parameter is null).
    IncompatibleTargetField, // Target struct field (array / slice elements) and AST field value are of
                             // incompatible types.
    BadReportType,           // Report struct field type doesn't match the target struct type.
                             // This should never happen, and is a sign of an internal problem in this library.
};

// ---------------------------------------------------------
// ---------------------- getFieldVal ----------------------
// ---------------------------------------------------------

/// Fetches the value of the field specified with dot-separated path from the AST.
/// T        - The type to be returned. Supported types are integers, floats, bool and []const u8
/// ast      - Abstract Syntax Tree to get the value from. Can be generated using std.zig.Ast.parse
/// fld_path - Dot-separated path to the field
pub fn getFieldVal(comptime T: type, ast: std.zig.Ast, fld_path: []const u8) !T
{
    const str_val = try getFieldValStr(ast, fld_path);
    switch (@typeInfo(T)) // tagged union: Type = union(enum)
    {
        .Int => |int_info|
        {
            // Requested type is u8 or u21 and field value is in a format that's not going to
            // make `std.zig.parseCharLiteral()` crash, so let's try calling it
            if ((int_info.bits == 8 or int_info.bits == 21) and int_info.signedness == .unsigned and
                str_val[0] == '\'' and str_val[str_val.len - 1] == '\'')
            {
                if (str_val.len == 3) { return str_val[1]; } // Unless it's just one byte between ' and '
                else
                {
                    if (str_val.len > 3 and str_val.len <= 6 and // str_val.len is not 3, so we must try calling `parseCharLiteral`
                        (str_val[1] == '\\' or str_val[1] == 0 or
                         ((str_val.len == 6 and str_val[1] & 0b11111000 == 0b11110000) or
                          (str_val.len == 5 and str_val[1] & 0b11110000 == 0b11100000) or
                          (str_val.len == 4 and str_val[1] & 0b11100000 == 0b11000000))))
                    {
                        const parsed_char = std.zig.parseCharLiteral(str_val);
                        switch (parsed_char)
                        {
                            .success =>
                            {
                                if (int_info.bits == 8 and parsed_char.success > 255)
                                {
                                    std.log.warn("Field '{s}': value {s} (0x{x}) can't fit the requested type u8",
                                                .{ fld_path, str_val, parsed_char.success });
                                    return ZonGetFieldsError.BadCharValue;
                                }
                                return if (int_info.bits == 8) @as(T, @intCast(parsed_char.success & 0x0000FF))
                                       else @as(T, @intCast(parsed_char.success)); // int_info.bits == 21
                            },
                            .failure =>
                            {
                                std.log.warn("Field '{s}': value {s} could not be parsed as a character : {}",
                                            .{ fld_path, str_val, parsed_char.failure });
                                return ZonGetFieldsError.BadCharValue;
                            }
                        }
                    }
                }
            }

            // Doesn't look like a character, try parsing it as an honest integer.
            // Autodetect the base, to allow hex, etc. --v
            const int_val = std.fmt.parseInt(T, str_val, 0) catch |err|
            {
                std.log.warn("Could not convert value of field '{s}' = '{s}' to type {s} : {}",
                             .{ fld_path, str_val, @typeName(T), err });
                return err;
            };
            return int_val;
        },
        .Float =>
        {
            const float_val = std.fmt.parseFloat(T, str_val) catch |err|
            {
                std.log.warn("Could not convert value of field '{s}' = '{s}' to type {s} : {}",
                             .{ fld_path, str_val, @typeName(T), err });
                return err;
            };
            return float_val;
        },
        .Bool =>
        {
            if (std.mem.eql(u8, str_val, "true")) { return true; }
            else
            {
                if (std.mem.eql(u8, str_val, "false")) { return false; }
                else
                {
                    std.log.warn("Value of field '{s}' = '{s}' is neither 'true' nor 'false'",
                                .{ fld_path, str_val });
                    return ZonGetFieldsError.BadBooleanValue;
                }
            }
        },
        .Pointer => |ptr_info|
        {
            if (ptr_info.size == .Slice and ptr_info.child == u8 and ptr_info.is_const) { return str_val; }
            else @compileError("fn getFieldVal: type '" ++ @typeName(T) ++ "' not supported");
        },
        else => @compileError("fn getFieldVal: type '" ++ @typeName(T) ++ "' not supported")
    }
}

/// Returns field value as a string - a slice of characters within `ast`.
/// This is the base function used `getFieldVal` before it converts the value to the type it needs.
fn getFieldValStr(ast: std.zig.Ast, fld_path: []const u8) ![]const u8
{
    var buf: [2]std.zig.Ast.Node.Index = undefined;
    var path_itr = std.mem.splitScalar(u8, fld_path, '.'); // SplitIterator(T, .scalar)
    const root_init = ast.fullStructInit(&buf, ast.nodes.items(.data)[0].lhs) orelse
    {
        std.log.warn("Zon parsing failed (top level struct)", .{});
        return ZonGetFieldsError.PathElementNotStruct;
    };
    var str_val = try walkAst(ast, root_init.ast.fields, &path_itr, 1);
    // Remove quotation marks if found
    if (str_val[0] == '"' and str_val[str_val.len - 1] == '"')
    {
        str_val.ptr += 1;
        str_val.len -= 2;
    }
    return str_val;
}

/// Meat and potatoes of the whole operation.
/// Goes through the elements of `ast`, specified by `ast_fields`, looking for the field
/// referenced by `path_itr.next()`. If `path_itr` contains no more elements after that,
/// the function returns field value, if it does, calls itself recursively.
/// In the end returns the value of the field defined by `path_itr` as a
/// slice of characters within `ast.source` (a string), or an error.
fn walkAst(ast: std.zig.Ast,
           ast_fields: []const std.zig.Ast.Node.Index,
           path_itr: *std.mem.SplitIterator(u8, .scalar),
           recursion_depth: u32) ![]const u8
{
    if (recursion_depth > zon_fld_path_len_limit)// Limit recursion depth, for the sake of sanity
    {
        std.log.warn("fn walkAst: recursion limit ({}) exceeded", .{ zon_fld_path_len_limit });
        return ZonGetFieldsError.PathLimitReached;
    }

    // `path_element` is the current path element this function is going to handle.
    // There are 4 options:
    // 1. `path_element` is the last element of the path, and refers to a scalar field
    //    `walkAst()` returns the value of this field;
    // 2. `path_element` is the last element of the path, and refers to an array element (ends with [<idx>]),
    //    `walkAst()` returns the value of this array element;
    // 3. `path_element` is not the last element of the path, and refers to a struct,
    //    `walkAst()` recursively calls itself;
    // 4. `path_element` is not the last element of the path, and refers to an array element (ends with [<idx>])
    //    `walkAst()` recursively calls itself.
    var path_element = path_itr.next() orelse
    {
        std.log.warn("Ran out of path elements", .{});
        return ZonGetFieldsError.NotFound;
    };
    if (path_element.len == 0)
    {
        std.log.warn("Path starts with dot or zero-length path", .{});
        return ZonGetFieldsError.BadSeparatorPosition;
    }

    // If this path element is an array element, we must figure out the index (arr_idx)
    const arr_idx: ?std.zig.Ast.Node.Index =
    blk:
    {
        if (path_element[path_element.len - 1] == ']')
        {
            if (std.mem.lastIndexOfScalar(u8, path_element, '[')) |left_brace_pos|
            {
                const arr_idx = std.fmt.parseInt(std.zig.Ast.Node.Index,
                                                 path_element[(left_brace_pos+1)..(path_element.len-1)], 0) catch |err|
                {
                    std.log.warn("Bad array index value in {s}, {}", .{ path_element, err });
                    return ZonGetFieldsError.BadArrIdxValue;
                };
                path_element.len = left_brace_pos; // Make sure we ignore the array index part from now on
                break :blk arr_idx;
            }
            else
            {
                std.log.warn("Bad array index syntax in {s}", .{ path_element });
                return ZonGetFieldsError.BadArrIdxSyntax;
            }
        }
        break :blk null; // Index not found, `path_element` does NOT refer to an element of an array
    };

    const fld_idx = blk2:
    {
        if (path_element.len == 0) // Is this a pure index (no field name)?
        {
            if (arr_idx) |arr_idx_val|
            {
                if (arr_idx_val < 0 or arr_idx_val > ast_fields.len - 1)
                {
                    std.log.warn("Array index out of bounds - unnamed array contains {d} elements, index is {d}",
                                 .{ ast_fields.len, arr_idx_val });
                    return ZonGetFieldsError.BadArrIdxValue;
                }
                break :blk2 ast_fields[arr_idx_val];
            }
            // path_element.len == 0, but also arr_idx == null - empty path_element
            std.log.warn("Path ends with dot, or two dots in a row", .{});
            return ZonGetFieldsError.BadSeparatorPosition;
        }

        // path_element.len != 0, must find the field by name
        for (ast_fields) |fld_idx|
        {
            const fld_name = ast.tokenSlice(ast.firstToken(fld_idx) - 2);
            if (std.mem.eql(u8, path_element, fld_name))
                break :blk2 fld_idx;
        }

        std.log.warn("Path element '{s}' not found", .{ path_element });
        return ZonGetFieldsError.NotFound;
    };

    // --== Now, figure out what do we do with the field (fld_idx) we have found ==--
    //
    // If there's more in the path, must treat current field as either a (sub) struct, or an array
    if (path_itr.peek()) |nxt| // Could have used if (path_itr_2.index) |_|, but need `len` too
    {
        if (nxt.len == 0)
        {
            std.log.warn("Path ends with dot, or two dots in a row", .{});
            return ZonGetFieldsError.BadSeparatorPosition;
        }

        if (arr_idx) |arr_idx_val| // There's index in this `path_element`, treat it as an array
        {
            // -----------------------------------------------------
            // 4. `path_element` is not the last element of the path,
            //    and refers to an array element (ends with [<idx>])
            // -----------------------------------------------------
            const node_idx = blk3: // std.zig.Ast.Node.Index
            {
                if (path_element.len == 0) // Pure index, no field name
                {
                    if (arr_idx_val < 0 or arr_idx_val > ast_fields.len - 1)
                    {
                        std.log.warn("Array index out of bounds - anonymous array contains {d} elements, index is {d}",
                                     .{ ast_fields.len, arr_idx_val });
                        return ZonGetFieldsError.BadArrIdxValue;
                    }
                    break :blk3 ast_fields[arr_idx_val];
                }

                // If we got here, remaining part of this `path_element` is NOT empty,
                // which means we must get this array field's elements before we can return one of them
                var buf: [2]std.zig.Ast.Node.Index = undefined;
                const arr_init = ast.fullArrayInit(&buf, fld_idx) orelse // ?full.ArrayInit
                {
                    std.log.warn("Parsing of field '{s}' failed, or value is not an array", .{ path_element });
                    return ZonGetFieldsError.PathElementNotArray;
                };
                if (arr_idx_val < 0 or arr_idx_val > arr_init.ast.elements.len - 1)
                {
                    std.log.warn("Array index out of bounds - '{s}' contains {d} elements, index is {d}",
                                 .{ path_element, arr_init.ast.elements.len, arr_idx_val });
                    return ZonGetFieldsError.BadArrIdxValue;
                }
                break :blk3 arr_init.ast.elements[arr_idx_val];
            };

            // Peek at the next path element to figure out how we should treat array element -
            // as a struct or as an array
            if (nxt[nxt.len - 1] == ']') // Array element is an array too
            {
                var buf: [2]std.zig.Ast.Node.Index = undefined;
                const arr_elt_arr_init = ast.fullArrayInit(&buf, node_idx) orelse
                {
                    std.log.warn("Parsing of field '{s}' failed, or its element {} is not an array",
                                .{ path_element, arr_idx_val });
                    return ZonGetFieldsError.PathElementNotArray;
                };
                return walkAst(ast, arr_elt_arr_init.ast.elements, path_itr, recursion_depth + 1);
            }
            else // Array element is a struct
            {
                var buf: [2]std.zig.Ast.Node.Index = undefined;
                const arr_elt_struct_init = ast.fullStructInit(&buf, node_idx) orelse
                {
                    std.log.warn("Parsing of element {} of array field '{s}' failed, or this element is not a struct",
                                .{ arr_idx_val, path_element });
                    return ZonGetFieldsError.PathElementNotStruct;
                };
                return walkAst(ast, arr_elt_struct_init.ast.fields, path_itr, recursion_depth + 1);
            }
        }
        else // No index in this `path_element`, treat it as a struct
        {
            // -----------------------------------------------------------------------------
            // 3. `path_element` is not the last element of the path, and refers to a struct
            // -----------------------------------------------------------------------------
            var buf: [2]std.zig.Ast.Node.Index = undefined;
            const substruct_init = ast.fullStructInit(&buf, fld_idx) orelse
            {
                std.log.warn("Parsing of field '{s}' failed, or value is not a struct", .{ path_element });
                return ZonGetFieldsError.PathElementNotStruct;
            };
            return walkAst(ast, substruct_init.ast.fields, path_itr, recursion_depth + 1);
        }
    }
    else // --== No more path elements after this one, we have arrived, return the value ==--
    {
        // Get array element `arr_idx`, if any
        if (arr_idx) |arr_idx_val|
        {
            // -----------------------------------------------------
            // 2. `path_element` is the last element of the path,
            //    and refers to an array element (ends with [<idx>])
            //    so we treat it as a scalar value,
            //    and return as a string
            // -----------------------------------------------------

            // Is the remaining part of this `path_element` is empty, meaning
            // `ast_fields` already contains the list of array element indices?
            if (path_element.len == 0)
            {
                if (arr_idx_val < 0 or arr_idx_val > ast_fields.len - 1)
                {
                    std.log.warn("Array index out of bounds - anonymous array contains {d} elements, index is {d}",
                                 .{ ast_fields.len, arr_idx_val });
                    return ZonGetFieldsError.BadArrIdxValue;
                }
                return getValueSlice(ast, ast.nodes.items(.main_token)[ast_fields[arr_idx_val]]);
            }

            // If we got here, remaining part of this `path_element` is NOT empty,
            // which means we must get this array field's elements before we can return one of them
            var buf: [2]std.zig.Ast.Node.Index = undefined;
            const arr_init = ast.fullArrayInit(&buf, fld_idx) orelse // ?full.ArrayInit
            {
                std.log.warn("Parsing of field '{s}' failed, or value is not an array", .{ path_element });
                return ZonGetFieldsError.PathElementNotArray;
            };
            if (arr_idx_val < 0 or arr_idx_val > arr_init.ast.elements.len - 1)
            {
                std.log.warn("Array index out of bounds - '{s}' contains {d} elements, index is {d}",
                             .{ path_element, arr_init.ast.elements.len, arr_idx_val });
                return ZonGetFieldsError.BadArrIdxValue;
            }

            const arr_elt_fld_idx = arr_init.ast.elements[arr_idx_val];
            return getValueSlice(ast, ast.nodes.items(.main_token)[arr_elt_fld_idx]);
        }
        // -------------------------------------------------------------------------------
        // 1. `path_element` is the last element of the path, and refers to a scalar field
        // -------------------------------------------------------------------------------
        // `arr_idx` is null, return the field value as a scalar
        return getValueSlice(ast, ast.nodes.items(.main_token)[fld_idx]);
    }
}

/// Wrapper around `std.zig.Ast.tokenSlice()`, to get negative numbers properly
fn getValueSlice(ast: std.zig.Ast, token_index: std.zig.Ast.TokenIndex) []const u8
{
    const ts = ast.tokenSlice(token_index);
    // Somehow for negative numbers (without quotation marks)
    // `Ast.tokenSlice` returns just the "-", so we must fix that.
    if (ts.len == 1 and ts[0] == '-')
    {
        // Ugly hack to get the correct slice
        var ts2 = ast.tokenSlice(token_index + 1);
        // We rely on the next tokenSlice being in `ast.source` right after
        // the one that is our "-".
        ts2.ptr -= 1;
        ts2.len += 1;
        return ts2;
    }
    return ts;
}

// ---------------------------------------------------------
// ------------------- getFieldVal Tests -------------------
// ---------------------------------------------------------

test "getFieldVal top level struct parsing error test"
{
    // Missing dot in .struct_1 = {
    const zon_txt =
        \\.{
        \\    .struct_1 =
        \\    {
        \\        .abc = "Hello",
        \\        .def = "you",
        \\    }
        \\}
        ;

    var ast = try std.zig.Ast.parse(std.testing.allocator, zon_txt, .zon);
    defer ast.deinit(std.testing.allocator);

    std.debug.print("\nDisregard the (warn) message below, it's expected and normal:\n", .{});
    try std.testing.expectError(error.PathElementNotStruct, getFieldVal([]const u8, ast, "struct_1.abc"));
}

test "getFieldVal big test"
{
    // Field order is not important
    const zon_txt =
        \\.{
        \\    .hex_u8 = 0x11, // Hexadecimal is OK
        \\    .str_u16 = "1991", // Number in a string is OK, as long as it's valid
        \\    .bin_u8 = 0b10010110, // Binary is OK too
        \\    .negative_i16 = -1000,
        \\    .struct_1 =
        \\    .{
        \\        .abc = "Hello",
        \\        .def = "you",
        \\        .str_no_quotes = i_am_a_string_too, // Not a normal Zig string, but this works too
        \\        .f32_cant_represent = 12.45, // See https://float.exposed , try this value
        \\        .f32_negative = -10.0,
        \\        .recursion = .{ .depth = .{ .limit = .{ .check = .{ .six = .{ .seven = .{ .eight = .{ .nine = .{ .ten = .{ .l11 = .{ .l12 = .{ .l13 = .{ .l14 = .{ .l15 = .{ .l16 = .{ .l17 = .{ .l18 = .{ .l19 = .{ .l20 = .{ .l21 = "TOO DEEP" }}}}}}}}}}}}}}}}}}},
        \\    },
        \\    .bool_1 = false,
        \\    .bool_2 = true,
        \\    .bool_str = "false",
        \\    .bool_bad = "dontknow",
        \\    .character_1 = 'A',
        \\    .character_escaped = '\'',
        \\    .character_newline = '\x0A', // aka '\n'
        \\    .character_no_quotes = B, // Bad
        \\    .character_unicode = '⚡',
        \\    //.character_bad = 'a whole string!', - Makes Ast.parse() fail
        \\    .str_unicode = "こんにちは",
        \\    .arr_u8 = .{ 10, 20, 30, 40 }, // This is an array
        \\    .arr_struct = // An array of structs
        \\    .{
        \\        .{ .abc = 12, .def = 34 },
        \\        .{ .abc = 56, .def = 78 },
        \\        .{ .abc = 90, .def = 12 },
        \\    },
        \\    .arr_arr = // Array of arrays
        \\    .{
        \\        .{ -11, -12, -13 },
        \\        .{  21,  22,  23 },
        \\        .{ -31, -32, -33 },
        \\    },
        \\    .arr_arr_struct = // Array of arrays of structs
        \\    .{
        \\        .{ .{ .q =  1, .w =  2 }, .{ .q =  3, .w =  4 }, .{ .q =  5, .w =  6 } },
        \\        .{ .{ .q =  7, .w =  8 }, .{ .q =  9, .w = 10 }, .{ .q = 11, .w = 12 } },
        \\        .{ .{ .q = 13, .w = 14 }, .{ .q = 15, .w = 16 }, .{ .q = 17, .w = 18 } },
        \\     },
        \\}
        ;
    var ast = try std.zig.Ast.parse(std.testing.allocator, zon_txt, .zon);
    defer ast.deinit(std.testing.allocator);

    std.debug.print("\nDisregard the (warn) messages below, they're expected and normal:\n", .{});

    try std.testing.expectError(error.BadSeparatorPosition, getFieldVal([]const u8, ast, "struct_1..abc"));
    try std.testing.expectError(error.BadSeparatorPosition, getFieldVal([]const u8, ast, ".struct_1.abc"));
    try std.testing.expectError(error.BadSeparatorPosition, getFieldVal([]const u8, ast, "struct_1.abc."));
    try std.testing.expectError(error.PathElementNotArray, getFieldVal([]const u8, ast, "struct_1[0]"));
    try std.testing.expectError(error.PathElementNotStruct, getFieldVal([]const u8, ast, "arr_arr[0].abc"));
    try std.testing.expectError(error.BadArrIdxValue, getFieldVal(u8, ast, "arr_u8[<bad index>]"));
    try std.testing.expectError(error.BadArrIdxSyntax, getFieldVal(u8, ast, "arr_u8{2]"));

    // Assuming zon_fld_path_len_limit == 20
    try std.testing.expect(zon_fld_path_len_limit <= 20);
    // Otherwise this test makes no sense and must be adjusted accordingly
    try std.testing.expectError(error.PathLimitReached,
                                getFieldVal([]const u8, ast,
                                            "struct_1.recursion.depth.limit.check.six.seven.eight." ++
                                            "nine.ten.l11.l12.l13.l14.l15.l16.l17.l18.l19.l20.l21"));

    var val_str = try getFieldVal([]const u8, ast, "hex_u8");
    try std.testing.expectEqualStrings(val_str, "0x11");
    //
    var val_u16 = try getFieldVal(u16, ast, "hex_u8");
    try std.testing.expectEqual(val_u16, 17);

    val_str = try getFieldVal([]const u8, ast, "str_u16");
    try std.testing.expectEqualStrings(val_str, "1991");
    //
    val_u16 = try getFieldVal(u16, ast, "str_u16");
    try std.testing.expectEqual(val_u16, 1991);
    //
    try std.testing.expectError(error.Overflow, getFieldVal(u8, ast, "str_u16"));

    var val_u8 = try getFieldVal(u8, ast, "bin_u8");
    try std.testing.expectEqual(val_u8, 0b10010110);

    const val_i16 = try getFieldVal(i16, ast, "negative_i16");
    try std.testing.expectEqual(val_i16, -1000);
    //
    val_str = try getFieldVal([]const u8, ast, "negative_i16");
    try std.testing.expectEqualStrings(val_str, "-1000");

    val_str = try getFieldVal([]const u8, ast, "struct_1.def");
    try std.testing.expectEqualStrings(val_str, "you");

    val_str = try getFieldVal([]const u8, ast, "struct_1.str_no_quotes");
    try std.testing.expectEqualStrings(val_str, "i_am_a_string_too");

    try std.testing.expectError(error.NotFound, getFieldVal(u8, ast, "struct_1.bad_field"));

    // See https://float.exposed , https://www.h-schmidt.net/FloatConverter/IEEE754.html
    var val_float = try getFieldVal(f32, ast, "struct_1.f32_cant_represent");
    try std.testing.expectEqual(val_float, 12.45);

    val_float = try getFieldVal(f32, ast, "struct_1.f32_negative");
    try std.testing.expectEqual(val_float, -10.0);

    var val_bool = try getFieldVal(bool, ast, "bool_1");
    try std.testing.expect(!val_bool);
    //
    val_bool = try getFieldVal(bool, ast, "bool_2");
    try std.testing.expect(val_bool);
    //
    val_bool = try getFieldVal(bool, ast, "bool_str");
    try std.testing.expect(!val_bool);
    //
    try std.testing.expectError(error.BadBooleanValue, getFieldVal(bool, ast, "bool_bad"));

    val_u8 = try getFieldVal(u8, ast, "character_1");
    try std.testing.expectEqual(val_u8, 'A');
    //
    val_u8 = try getFieldVal(u8, ast, "character_escaped");
    try std.testing.expectEqual(val_u8, '\'');
    //
    val_u8 = try getFieldVal(u8, ast, "character_newline");
    try std.testing.expectEqual(val_u8, '\n');
    //
    try std.testing.expectError(error.InvalidCharacter, getFieldVal(u8, ast, "character_no_quotes"));
    try std.testing.expectError(error.InvalidCharacter, getFieldVal(u8, ast, "struct_1.str_no_quotes"));
    try std.testing.expectError(error.BadCharValue, getFieldVal(u8, ast, "character_unicode"));

    const val_u21 = try getFieldVal(u21, ast, "character_unicode");
    try std.testing.expectEqual(val_u21, '⚡');

    val_str = try getFieldVal([]const u8, ast, "str_unicode");
    try std.testing.expectEqualStrings(val_str, "こんにちは");

    val_u8 = try getFieldVal(u8, ast, "arr_u8[3]");
    try std.testing.expectEqual(val_u8, 40);
    //
    try std.testing.expectError(error.BadArrIdxValue, getFieldVal(u8, ast, "arr_u8[4]"));

    val_u8 = try getFieldVal(u8, ast, "arr_struct[2].abc");
    try std.testing.expectEqual(val_u8, 90);
    //
    try std.testing.expectError(error.NotFound, getFieldVal(u8, ast, "arr_struct[2].not_there"));

    val_str = try getFieldVal([]const u8, ast, "arr_arr[2].[2]");
    try std.testing.expectEqualStrings(val_str, "-33");
    //
    const val_i8 = try getFieldVal(i8, ast, "arr_arr[2].[2]");
    try std.testing.expectEqual(val_i8, -33);

    val_u8 = try getFieldVal(u8, ast, "arr_arr_struct[2].[1].q");
    try std.testing.expectEqual(val_u8, 15);
}

// ---------------------------------------------------------
// ---------------------- zonToStruct ----------------------
// ---------------------------------------------------------

/// Was the field / array element / slice element filled or not
pub const ZonFieldResult = enum
{
    not_filled,       // Field / array element / slice element was not filled
    partially_filled, // This element of array or slice was filled, but not all elements were filled
    filled,           // Field / array / slice was filled
    overflow_filled,  // This element of array or slice was filled, and all others as well, but there
                      // were too many data elements in AST, and they were ignored
    fn isGreaterThan(self: ZonFieldResult, other: ZonFieldResult) bool
    {
        return switch (other)
        {
            .not_filled       => self == .partially_filled or self == .filled or self == .overflow_filled,
            .partially_filled => self == .filled or self == .overflow_filled,
            .filled           => self == .overflow_filled,
            .overflow_filled  => false
        };
    }
};

/// Populate target struct fields with values from `ast`.
/// If a field has no counterpart in `ast`, it's not going to be filled.
/// `tgt_struct_ptr` Must be a pointer to a mutable (non-const) target struct instance.
/// `ast`            Abstract syntax tree generated from the ZON source text.
/// `allocr`         Optional allocator. May be null, but in this case target struct can't contain
///                  slices, except for `[]const u8` - those will point to string values within `ast.source`
///                  (or those slices have no counterparts in `ast` so they are ignored).
pub fn zonToStruct(tgt_struct_ptr: anytype, ast: std.zig.Ast, allocr: ?std.mem.Allocator)
    !MakeReportStructType(if (@typeInfo(@TypeOf(tgt_struct_ptr)) != .Pointer or
                              @typeInfo(@TypeOf(tgt_struct_ptr)).Pointer.is_const or
                              @typeInfo(@TypeOf(tgt_struct_ptr)).Pointer.size != .One or
                              @typeInfo(@typeInfo(@TypeOf(tgt_struct_ptr)).Pointer.child) != .Struct)
                              @compileError("fn zonToStruct: `tgt_struct_ptr` is " ++ @typeName(@TypeOf(tgt_struct_ptr)) ++
                                            " , expected a single-item pointer to a mutable (non-const) struct")
                          else
                              @typeInfo(@TypeOf(tgt_struct_ptr)).Pointer.child)
{
    // Root field index -----------------------------------------------------v
    return try populateStruct(tgt_struct_ptr, ast, ast.nodes.items(.data)[0].lhs,
                              ZonFieldResult.filled, allocr, 0);
}

/// Returns report struct (field) type that represents this target struct field type, `tgt_type`,
/// which is a struct (including the target struct itselg). See `Report struct field types` in README.md
fn MakeReportStructType(tgt_type: type) type
{
    const tgt_type_info = @typeInfo(tgt_type);
    if (tgt_type_info != .Struct)
        @compileError("fn MakeReportStructType: `tgt_type` is " ++ @typeName(tgt_type) ++
                      " , expected a struct");
    const flds =
    blk:
    {
        var non_comptime_fld_count = 0; // Count non-comptime fields of the target struct
        for (tgt_type_info.Struct.fields) |tgt_fld|
        {
            if (!tgt_fld.is_comptime)
                non_comptime_fld_count += 1;
        }

        var flds_values: [non_comptime_fld_count]std.builtin.Type.StructField = undefined;
        var i = 0; // comptime_int
//         const unfilled = ZonFieldResult.not_filled;
        for (tgt_type_info.Struct.fields) |tgt_fld|
        {
            if (!tgt_fld.is_comptime)
            {
                flds_values[i] =
                .{
                    .name = tgt_fld.name,
                    .type =
                        switch (@typeInfo(tgt_fld.type))
                        {
                            .Struct => MakeReportStructType(tgt_fld.type),
                            .Array => MakeReportArrayType(tgt_fld.type),
                            .Pointer => |ptr_info|
                                if (ptr_info.size == .Slice)
                                    MakeReportSliceType(tgt_fld.type)
                                else
                                    ZonFieldResult,
                            else => ZonFieldResult,
                        },
                    .default_value = null,
//                     .default_value = if (@typeInfo(tgt_fld.type) != .Struct and @typeInfo(tgt_fld.type) != .Array)
//                                          &unfilled //@as(?*ZonFieldResult, &unfilled)
//                                      else
//                                          null,
                    .is_comptime = false,
                    .alignment =
                        @alignOf(switch (@typeInfo(tgt_fld.type))
                                 {
                                     .Struct => MakeReportStructType(tgt_fld.type),
                                     .Array => MakeReportArrayType(tgt_fld.type),
                                     .Pointer => |ptr_info|
                                         if (ptr_info.size == .Slice)
                                             MakeReportSliceType(tgt_fld.type)
                                         else
                                             ZonFieldResult,
                                     else => ZonFieldResult
                                 }),
                };
                i += 1;
            }
        }
        break :blk flds_values;
    };

    return @Type(.{
                    .Struct =
                    .{
                        .layout = .auto,
                        .backing_integer = null,
                        .fields = &flds,
                        .decls = &.{},
                        .is_tuple = false,
                    }
                 });
}

/// Returns report struct field type that represents this target struct field type, `tgt_type`,
/// which is an array. See `Report struct field types` in README.md
fn MakeReportArrayType(tgt_type: type) type
{
    const tgt_type_info = @typeInfo(tgt_type);
    if (tgt_type_info != .Array)
        @compileError("fn MakeReportArrayType: `tgt_type` is " ++ @typeName(tgt_type) ++
                      " , expected an array");
    return @Type(.{
                    .Array =
                    .{
                        .len = tgt_type_info.Array.len,
                        .child =
                            switch(@typeInfo(tgt_type_info.Array.child))
                            {
                                .Array => MakeReportArrayType(tgt_type_info.Array.child),
                                .Struct => MakeReportStructType(tgt_type_info.Array.child),
                                .Pointer => |ptr_info|
                                    if (ptr_info.size == .Slice)
                                        MakeReportSliceType(tgt_type_info.Array.child)
                                    else
                                        ZonFieldResult,
                                else => ZonFieldResult,
                            },
                        .sentinel = null,
                    }
                 });
}

/// Returns report struct field type that represents this target struct field type, `tgt_type`,
/// which is a slice. See `Report struct field types` in README.md
fn MakeReportSliceType(tgt_type: type) type
{
    const tgt_type_info = @typeInfo(tgt_type);
    if (tgt_type_info != .Pointer)
        @compileError("fn MakeReportSliceType: `tgt_type` is " ++ @typeName(tgt_type) ++
                      " , expected a slice");
    if (tgt_type_info.Pointer.size != .Slice)
        @compileError("fn MakeReportSliceType: `tgt_type` is a Pointer: " ++ @typeName(tgt_type) ++
                      " , expected a slice");

    // Only target slices of structs, arrays and slices are represented in the report struct
    // with slices of ZonFieldResult.
    // Slices of primitive types, such as integers, floats, etc., and even pointers that are not slices,
    // are represented with a single ZonFieldResult field the report struct.
    const ReportSliceChildType =
        switch (@typeInfo(tgt_type_info.Pointer.child))
        {
            .Struct => MakeReportStructType(tgt_type_info.Pointer.child),
            .Array => MakeReportArrayType(tgt_type_info.Pointer.child),
            .Pointer => |ptr_info|
                if (ptr_info.size == .Slice)
                    MakeReportSliceType(tgt_type_info.Pointer.child)
                else
                    ZonFieldResult, // Slice elemetns are primitive pointers (not slices), though we actually don't support these target struct fields o_O
            else => return ZonFieldResult // Slice elemetns are of a primitive type, so in the report we'll have a single ZonFieldResult
        };

    // We've got a 'fancier' slice in the target struct - its elements are either structs, arrays, or slices.
    return @Type(.{
                    .Pointer =
                    .{
                        .size = .Slice,
                        .is_const = false,
                        .is_volatile = false,
                        .alignment = @alignOf(ReportSliceChildType), // TODO: SHOULDN'T IT BE SLICE ALIGNMENT, NOT SLICE ELEMENT ALIGNMENT? -- BUT HOW?
                        .address_space = .generic, // ? pub const AddressSpace = enum(u5) { // CPU address spaces.  generic, gs, fs, ss, ...
                        .child = ReportSliceChildType,
                        .is_allowzero = false,
                        .sentinel = null,
                    }
                });
}

/// Create and return an initialized instance of the report struct (of type T).
/// Struct fields are initialized with ZonFieldResult.not_filled, including nested structs and arrays.
/// `T` struct type. This is the type returned by MakeReportStructType function.
/// `allocr` allocator is there to be passed to `makeReportSlice`.
fn makeReportStruct(T: type) T
{
    if (@typeInfo(T) != .Struct)
        @compileError("fn makeReportStruct: `T` is " ++ @typeName(T) ++ " , expected a struct");

    var res: T = undefined;
    inline for (@typeInfo(T).Struct.fields) |fld|
        if (!fld.is_comptime)
        {
            if (fld.type == ZonFieldResult) { @field(res, fld.name) = ZonFieldResult.not_filled; }
            else
                switch (@typeInfo(fld.type))
                {
                    .Struct => @field(res, fld.name) = makeReportStruct(fld.type),
                    .Array  => @field(res, fld.name) = makeReportArray(fld.type),
                    .Pointer => |ptr_info|
                        if (ptr_info.size == .Slice)
                        {   @field(res, fld.name) = makeReportSlice(fld.type);   }
                        else
                            @compileError("fn makeReportStruct: field `" ++ fld.name ++ "` is a Pointer, but not a slice: " ++
                                          @typeName(fld.type) ++ " , expected ZonFieldResult, struct, array or slice"),
                    else => @compileError("fn makeReportStruct: field `" ++ fld.name ++ "` is " ++
                                          @typeName(fld.type) ++ " , expected ZonFieldResult, struct, array or slice")
                }
        };
    return res;
}

/// Create and return an initialized instance of the report array (of type T).
/// Array elements are initialized to to ZonFieldResult.not_filled, including nested structs and arrays.
/// `T` array type. This is the type returned by MakeReportArrayType function.
/// `allocr` allocator is there to be passed to `makeReportSlice`.
fn makeReportArray(T: type) T
{
    if (@typeInfo(T) != .Array)
        @compileError("fn makeReportArray: `T` is " ++ @typeName(T) ++ " , expected an array");

    var res: T = undefined;
    if (@typeInfo(T).Array.child == ZonFieldResult)
    {
        for (&res) |*e| e.* = ZonFieldResult.not_filled;
        return res;
    }
    // Array elements are NOT simple ZonFieldResult enums
    const arr_elt_type_info = @typeInfo(@typeInfo(T).Array.child);
    switch (arr_elt_type_info)
    {
        .Struct => { for (&res) |*e| e.* = makeReportStruct(@typeInfo(T).Array.child); },
        .Array  => { for (&res) |*e| e.* = makeReportArray(@typeInfo(T).Array.child); },
        .Pointer => |ptr_info|
            if (ptr_info.size == .Slice)
            {   for (&res) |*e| e.* = makeReportSlice(@typeInfo(T).Array.child);   }
            else
                @compileError("fn makeReportArray: array elemets are `Pointer`s, but not slices" ++
                              @typeName(@typeInfo(T).Array.child) ++
                              " , expected ZonFieldResult, struct, array or slice"),
        else => @compileError("fn makeReportArray: array elemets are " ++
                              @typeName(@typeInfo(T).Array.child) ++
                              " , expected ZonFieldResult, struct, array or slice")
    }
    return res;
}

/// Create and return an initialized instance of the report slice (of type T).
/// Array elements are initialized to to ZonFieldResult.not_filled, including nested structs and arrays.
/// `T` slice type. This is the type returned by MakeReportSliceType function.
fn makeReportSlice(T: type) T
{
    if (T == ZonFieldResult)
        return ZonFieldResult.not_filled;

    const tgt_type_info = @typeInfo(T);
    if (tgt_type_info != .Pointer)
        @compileError("fn makeReportSlice: `T` is " ++ @typeName(T) ++ " , expected a ZonFieldResult or a slice");
    if (tgt_type_info.Pointer.size != .Slice)
        @compileError("fn makeReportSlice: `T` is a Pointer: " ++ @typeName(T) ++ " , expected a slice");

    return &.{};
}

/// Populate target (sub)struct with values from `ast`, starting with `struct_ast_fld_idx`.
/// If a field has no counterpart in `ast`, it's not going to be filled.
/// `ptr_tgt`            Must be a pointer to a mutable (non-const) target struct instance.
/// `ast`                Abstract syntax tree generated from the ZON source text.
/// `struct_ast_fld_idx` Index of the root node of the source (sub)tree in `ast`
/// `rept_filled_val`    Value to be assigned to report struct fields, if the value is found in AST
/// `allocr`             Optional allocator. May be null, but in this case target struct can't
///                      contain slices, except for `[]const u8` - those will point to string values
///                      within `ast.source` (or those slices have no counterparts in `ast` and are ignored).
/// `recursion_depth`    Recursion level, on the first call should be 1. Incremented on subsequent calls
///                      to `populateSlice`, `populateArray`, `populateStruct`.
fn populateStruct(ptr_tgt: anytype,
                  ast: std.zig.Ast,
                  struct_ast_fld_idx: std.zig.Ast.Node.Index,
                  rept_filled_val: ZonFieldResult,
                  allocr: ?std.mem.Allocator,
                  comptime recursion_depth: u32)
    !MakeReportStructType(if (@typeInfo(@TypeOf(ptr_tgt)) != .Pointer or
                              @typeInfo(@TypeOf(ptr_tgt)).Pointer.is_const or
                              @typeInfo(@TypeOf(ptr_tgt)).Pointer.size != .One or
                              @typeInfo(@typeInfo(@TypeOf(ptr_tgt)).Pointer.child) != .Struct)
                              @compileError("fn populateStruct: `ptr_tgt` is " ++ @typeName(@TypeOf(ptr_tgt)) ++
                                            " , expected a single-item pointer to a mutable (non-const) struct")
                          else
                              @typeInfo(@TypeOf(ptr_tgt)).Pointer.child)
{
    if (recursion_depth > zon_fld_path_len_limit) // Limit recursion depth, for the sake of sanity
    {
        @compileLog("zon_fld_path_len_limit = ", zon_fld_path_len_limit);
        @compileLog("recursion_depth = ", recursion_depth);
        @compileError("fn populateStruct: recursion limit exceeded");
    }

    var buf: [2]std.zig.Ast.Node.Index = undefined;
    const struct_init = ast.fullStructInit(&buf, struct_ast_fld_idx) orelse
    {
        if (struct_ast_fld_idx == ast.nodes.items(.data)[0].lhs) // Root element
        {
            std.log.warn("Zon root structure parsing failed, is it a struct?", .{});
        }
        else
        {
            const ast_fld_name = ast.tokenSlice(ast.firstToken(struct_ast_fld_idx) - 2);
            std.log.warn("Zon field {s} parsing failed, is it a struct?", .{ ast_fld_name });
        }
        return ZonGetFieldsError.PathElementNotStruct;
    };

    var report = makeReportStruct(MakeReportStructType(@typeInfo(@TypeOf(ptr_tgt)).Pointer.child));

    const tgt_type_info = @typeInfo(@TypeOf(ptr_tgt.*));
    // This unrolls as many times as the number of fields in ptr_tgt.*
    inline for (tgt_type_info.Struct.fields) |tgt_fld|
    {
        if (!tgt_fld.is_comptime) // Skip comptime fields - this check is comptime
        {
            // Initialize the field with ZonFieldResult.not_filled, including all the nested structs, arrays, etc.
            for (struct_init.ast.fields) |ast_fld_idx|
            {
                const ast_fld_name = ast.tokenSlice(ast.firstToken(ast_fld_idx) - 2);
                if (std.mem.eql(u8, tgt_fld.name, ast_fld_name))
                    switch (@typeInfo(tgt_fld.type))
                    {
                        .Int =>
                        {
                            @field(ptr_tgt.*, tgt_fld.name) = try getValueInt(tgt_fld.type, ast, ast_fld_idx);
                            @field(report, tgt_fld.name) = rept_filled_val; //ZonFieldResult.filled;
                        },
                        .Float =>
                        {
                            @field(ptr_tgt.*, tgt_fld.name) = try getValueFloat(tgt_fld.type, ast, ast_fld_idx);
                            @field(report, tgt_fld.name) = rept_filled_val; //ZonFieldResult.filled;
                        },
                        .Bool =>
                        {
                            @field(ptr_tgt.*, tgt_fld.name) = try getValueBool(ast, ast_fld_idx);
                            @field(report, tgt_fld.name) = rept_filled_val; //ZonFieldResult.filled;
                        },
                        .Struct =>
                            @field(report, tgt_fld.name) =
                                try populateStruct(&@field(ptr_tgt.*, tgt_fld.name), ast,
                                                   ast_fld_idx, ZonFieldResult.filled,
                                                   allocr, recursion_depth + 1),
                        .Array => // Target struct field is an array
                            @field(report, tgt_fld.name) =
                                try populateArray(&@field(ptr_tgt.*, tgt_fld.name), ast, ast_fld_idx,
                                                  allocr, recursion_depth + 1),
                        .Pointer => |ptr_info|
                        {
                            if (ptr_info.size == .Slice) // Target struct field is a slice
                            {
                                // `fn populateSlice()` returns its result via one of its arguments,
                                // `ptr_report`. This is because depending on the type of target slice elements,
                                // the result can be either a single ZonFieldResult enum (for primitive elements),
                                // or a slice of nested structs, arrays, slices that contain ZonFieldResult enums
                                // somewhere at the bottom.
                                // To be able to return the result, `ptr_report` must be a pointer to the var of
                                // the proper type.
                                const PopSliceResultType = MakeReportSliceType(tgt_fld.type);
                                var pop_slc_rslt = makeReportSlice(PopSliceResultType); // returns &.{} or ZonFieldResult.not_filled
                                // Pass a pointer to this slice field to `populateSlice`.
                                try populateSlice(&@field(ptr_tgt.*, tgt_fld.name), ast, ast_fld_idx,
                                                  allocr, &pop_slc_rslt, recursion_depth + 1);
                                @field(report, tgt_fld.name) = pop_slc_rslt;
                            }
                            else
                                std.log.warn("Struct field '" ++ tgt_fld.name ++ "' of pointer type " ++
                                             tgt_fld.type ++ " found - not supported, skipping");
                        },
                        else => std.log.warn("Struct field '" ++ tgt_fld.name ++ "' of type " ++
                                            tgt_fld.type ++ " found - not supported, skipping"),
                    };
            }
        }
    }
    return report;
}

/// Populate target array `ptr_tgt_arr` points to with values from array in `ast`, at index
/// `arr_ast_fld_idx`. Normally used to populate arrays within the target struct.
/// If `ptr_tgt_arr.*` contains less (or more) elements than its counterpart in `ast`,
/// excess elements will be ignored / left unchaged.
/// `ptr_tgt_arr`     Pointer to array of mutable (non-const) elements
/// `ast`             Abstract syntax tree generated from the ZON source text.
/// `arr_ast_fld_idx` Index of the array node in `ast`.
/// `allocr`          Optional allocator. Not used in `fn populateArray`,
///                   but is passed to `fn populateStruct` and `fn populateSlice`.
///                   May be null, but in this case target struct can't contain slices, except for
///                   `[]const u8` - those will point to string values within `ast.source`
///                   (or those slices have no counterparts in `ast` so they are ignored).
/// `recursion_depth` Recursion level, on the first call should be 1. Incremented on subsequent calls
///                   to `populateSlice`, `populateArray`, `populateStruct`.
fn populateArray(ptr_tgt_arr: anytype,
                 ast: std.zig.Ast,
                 arr_ast_fld_idx: std.zig.Ast.Node.Index,
                 allocr: ?std.mem.Allocator,
                 comptime recursion_depth: u32)
    !MakeReportArrayType(if (@typeInfo(@TypeOf(ptr_tgt_arr)) != .Pointer or
                          @typeInfo(@TypeOf(ptr_tgt_arr)).Pointer.is_const or
                          @typeInfo(@TypeOf(ptr_tgt_arr)).Pointer.size != .One or
                          @typeInfo(@typeInfo(@TypeOf(ptr_tgt_arr)).Pointer.child) != .Array)
                          @compileError("fn populateArray: `ptr_tgt_arr` is " ++ @typeName(@TypeOf(ptr_tgt_arr)) ++
                                        " , expected a single-item pointer to a non-const (mutable) array")
                      else
                          @typeInfo(@TypeOf(ptr_tgt_arr)).Pointer.child)
{
    if (recursion_depth > zon_fld_path_len_limit) // Limit recursion depth, for the sake of sanity
    {
        @compileLog("zon_fld_path_len_limit = ", zon_fld_path_len_limit);
        @compileLog("recursion_depth = ", recursion_depth);
        @compileError("fn populateArray: recursion limit exceeded");
    }

    const ptr_tgt_arr_type_info = @typeInfo(@TypeOf(ptr_tgt_arr));
    const arr_type_info = @typeInfo(ptr_tgt_arr_type_info.Pointer.child);
    const arr_elt_type_info = @typeInfo(arr_type_info.Array.child);

    var report = makeReportArray(MakeReportArrayType(ptr_tgt_arr_type_info.Pointer.child));

    // Try parsing found AST field as array, and if it doesn't work, try treating it as a string
    var buf: [2]std.zig.Ast.Node.Index = undefined;
    const arr_init = ast.fullArrayInit(&buf, arr_ast_fld_idx) orelse // ?full.ArrayInit
    {
        // Perhaps it's a string?
        var strv = getValueSlice(ast, ast.nodes.items(.main_token)[arr_ast_fld_idx]);
        if (strv.len > 1 and strv[0] == '"' and strv[strv.len - 1] == '"')
        {
            // It's a string, remove quotation marks
            strv.ptr += 1;
            strv.len -= 2;

            // If `ptr_tgt_arr` pointe to an array of u8's, we could copy strv string into this array
            if (arr_type_info.Array.child == u8)
            {
                // Copy as many bytes from the AST field into the target struct field as possible,
                // filling the remainder with 0 bytes. Result value is:
                // "filled" if lengths of the target array and the value string are the same;
                // "overflow_filled" if the value string is longer than the target array;
                // "partially_filled" if the target array is longer than the value string.
                const rept_val = if (ptr_tgt_arr.*.len > strv.len) ZonFieldResult.partially_filled
                                 else
                                     if (ptr_tgt_arr.*.len < strv.len) ZonFieldResult.overflow_filled
                                     else ZonFieldResult.filled;
                for (ptr_tgt_arr, &report, 0..) |*tgt_el, *rept_el, i|
                {
                    tgt_el.* = if (i < strv.len) strv[i] else 0; // 0 if the value string ran out of characters
                    rept_el.* = rept_val;
                }
                return report; // Done with this field
            }

            const ast_fld_name = ast.tokenSlice(ast.firstToken(arr_ast_fld_idx) - 2);
            std.log.warn("Value of field '{s}' is a string = '{s}' " ++
                         "but target array elements ({s}) are not u8",
                         .{ ast_fld_name, strv, @typeName(arr_type_info.Array.child) });
            return ZonGetFieldsError.IncompatibleTargetField;
        }
        // Value is not a string (no double quotation marks), it's an error
        const ast_fld_name = ast.tokenSlice(ast.firstToken(arr_ast_fld_idx) - 2);
        std.log.warn("Parsing of field '{s}' failed, or value is not an array: {s}\n, and ",
                     .{ ast_fld_name, strv });
        return ZonGetFieldsError.PathElementNotArray;
    };
    // At this point, we're sure we have parsed AST element `arr_ast_fld_idx` as array

    const elems_to_copy =
        if (arr_init.ast.elements.len < ptr_tgt_arr.*.len) arr_init.ast.elements.len
        else ptr_tgt_arr.*.len;
    // Result value is:
    // "filled" if lengths of the target array and the value string are the same;
    // "overflow_filled" if the value string is longer than the target array;
    // "partially_filled" if the target array is longer than the value string.
    const rept_val = if (ptr_tgt_arr.*.len > arr_init.ast.elements.len) ZonFieldResult.partially_filled
                     else
                         if (ptr_tgt_arr.*.len < arr_init.ast.elements.len) ZonFieldResult.overflow_filled
                         else ZonFieldResult.filled;
    for (0..elems_to_copy) |i|
    {
        const ast_arr_elt_fld_idx = arr_init.ast.elements[i];
        // Maybe have for loop nested in this comptime switch instead?
        switch (arr_elt_type_info)
        {
            .Int =>
            {
                ptr_tgt_arr[i] = try getValueInt(arr_type_info.Array.child, ast, ast_arr_elt_fld_idx);
                report[i] = rept_val;
            },
            .Float =>
            {
                ptr_tgt_arr[i] = try getValueFloat(arr_type_info.Array.child, ast, ast_arr_elt_fld_idx);
                report[i] = rept_val;
            },
            .Bool =>
            {
                ptr_tgt_arr[i] = try getValueBool(ast, ast_arr_elt_fld_idx);
                report[i] = rept_val;
            },
            .Struct => report[i] = try populateStruct(&ptr_tgt_arr[i], ast, ast_arr_elt_fld_idx,
                                                      rept_val, allocr, recursion_depth + 1),
            .Array => report[i] = try populateArray(&ptr_tgt_arr[i], ast, ast_arr_elt_fld_idx,
                                                    allocr, recursion_depth + 1),
            .Pointer => |ptr_info|
            {
                if (ptr_info.size == .Slice) // Target struct field is a slice
                {
                    // `fn populateSlice()` returns its result via one of its arguments,
                    // `ptr_report`. This is because depending on the type of target slice elements,
                    // the result can be either a single ZonFieldResult enum (for primitive elements),
                    // or a slice of nested structs, arrays, slices that contain ZonFieldResult enums
                    // somewhere at the bottom.
                    // To be able to return the result, `ptr_report` must be a pointer to the var of
                    // the proper type.
                    const PopSliceResultType = MakeReportSliceType(arr_type_info.Array.child);
                    var pop_slc_rslt = makeReportSlice(PopSliceResultType); // returns &.{} currently
                    // Pass a pointer to this slice field to `populateSlice`.
                    try populateSlice(&ptr_tgt_arr[i], ast, ast_arr_elt_fld_idx, allocr,
                                      &pop_slc_rslt, recursion_depth + 1);
                    report[i] = pop_slc_rslt;
                }
                else
                {
                    report[i] = ZonFieldResult.not_filled;
                    std.log.warn("Array elements of pointer type " ++
                                 @typeName(ptr_tgt_arr_type_info.Pointer.child) ++ " are not supported, skipping", .{});
                }
            },
            else => @compileError("Arrrays of " ++ @typeName(ptr_tgt_arr_type_info.Pointer.child) ++
                                  " are not supported")
        }
    }
    return report;
}

/// Populate target slice `ptr_tgt_slc` is a pointer to, with values from `ast`, at index `arr_ast_fld_idx`.
/// A new buffer is allocated using `allocr` allocator, and the caller then owns this buffer;
/// EXCEPT when `ptr_tgt_slc` is *[]const u8 and the value in `ast` is a string (double quotation marks),
/// in which case `ptr_tgt_slc.*`'s ptr and len are set to the value string within `ast.source`.
/// `ptr_tgt_slc`     Pointer to the target slice. If `ptr_tgt_slc` is *[]const u8, `ptr_tgt_slc.*`
///                   is assigned the address/length of the zon value string in `ast.source`, otherwise
///                   a new buffer is allocated using `allocr`, and the values are copied to it.
///                   The caller will own this buffer.
/// `ast`             Abstract syntax tree generated from the ZON source text.
/// `arr_ast_fld_idx` Index of the array node in `ast`.
/// `allocr`          Optional allocator. Must be non-null unless `ptr_tgt_slc` is *[]const u8,
///                   otherwise an error is returned.
/// `ptr_report`      A non-const single-item pointer to ZonFieldResult (for `ptr_tgt_slc` of primitive
///                   elements, or to a slice (for `ptr_tgt_slc` of structs, arrays or slices).
///                   In the latter case, memory for this report slice will be allocated using `allocr`.
///                   The caller will own this memory and will have to appropriately deallocate it.
/// `recursion_depth` Recursion level, on the first call should be 1. Incremented on subsequent calls
///                   to `populateSlice`, `populateArray`, `populateStruct`.
fn populateSlice(ptr_tgt_slc: anytype,
                 ast: std.zig.Ast,
                 arr_ast_fld_idx: std.zig.Ast.Node.Index,
                 allocr: ?std.mem.Allocator,
                 ptr_report: anytype,
                 comptime recursion_depth: u32) !void
{
    // Comptime check type of ptr_tgt_slc
    const ptr_tgt_slc_typ_inf = @typeInfo(@TypeOf(ptr_tgt_slc));
    if (ptr_tgt_slc_typ_inf != .Pointer or ptr_tgt_slc_typ_inf.Pointer.size != .One or // Not a single-item pointer
        @typeInfo(ptr_tgt_slc_typ_inf.Pointer.child) != .Pointer or              // to
        @typeInfo(ptr_tgt_slc_typ_inf.Pointer.child).Pointer.size != .Slice)     // slice
        @compileError("fn populateSlice: `ptr_tgt_slc` is " ++ @typeName(@TypeOf(ptr_tgt_slc)) ++
                      " , expected a single-item pointer to slice");

    // Comptime check type of report
    const report_typ_inf = @typeInfo(@TypeOf(ptr_report));
    const tgt_slice_child_type = @typeInfo(ptr_tgt_slc_typ_inf.Pointer.child).Pointer.child;
    const tgt_slc_chld_typ_inf = @typeInfo(tgt_slice_child_type);
    if ((tgt_slc_chld_typ_inf == .Int or
         tgt_slc_chld_typ_inf == .Float or
         tgt_slc_chld_typ_inf == .Bool) and // Target slice elements are of a primitive type
        (report_typ_inf != .Pointer or report_typ_inf.Pointer.size != .One or // Not a single-item pointer
         report_typ_inf.Pointer.is_const or                                   // or a const pointer
         report_typ_inf.Pointer.child != ZonFieldResult))                     // or not to ZonFieldResult
        @compileError("fn populateSlice: `ptr_report` is " ++ @typeName(@TypeOf(ptr_report)) ++
                      " , expected a non-const single-item pointer to ZonFieldResult, since target slice " ++
                      "elements are of primitive type " ++ @typeName(tgt_slice_child_type));
    //
    if ((tgt_slc_chld_typ_inf == .Struct or
         tgt_slc_chld_typ_inf == .Array or
         tgt_slc_chld_typ_inf == .Pointer) and // Target slice elements are of a complex type
        (report_typ_inf != .Pointer or report_typ_inf.Pointer.size != .One or // Not a single-item pointer
         report_typ_inf.Pointer.is_const or                                   // or a const pointer
         @typeInfo(report_typ_inf.Pointer.child) != .Pointer or               // or not..
         @typeInfo(report_typ_inf.Pointer.child).Pointer.size != .Slice))     // ..to a slice
        @compileError("fn populateSlice: `ptr_report` is " ++ @typeName(@TypeOf(ptr_report)) ++
                      " , expected a non-const single-item pointer to slice, since target slice " ++
                      "elements are of complex type " ++ @typeName(tgt_slice_child_type));

    if (recursion_depth > zon_fld_path_len_limit) // Limit recursion depth, for the sake of sanity
    {
        @compileLog("zon_fld_path_len_limit = ", zon_fld_path_len_limit);
        @compileLog("recursion_depth = ", recursion_depth);
        @compileError("fn populateSlice: recursion limit exceeded");
    }

    // Try parsing found AST field as array, and if that doesn't work, try treating it as a string
    var buf: [2]std.zig.Ast.Node.Index = undefined;
    const arr_init = ast.fullArrayInit(&buf, arr_ast_fld_idx) orelse // ?full.ArrayInit
    {
        // Perhaps it's a string?
        var strv = getValueSlice(ast, ast.nodes.items(.main_token)[arr_ast_fld_idx]);
        if (strv.len > 1 and strv[0] == '"' and strv[strv.len - 1] == '"')
        {
            // It's a string, remove quotation marks
            strv.ptr += 1;
            strv.len -= 2;

            if (tgt_slice_child_type == u8)
            {
                if (report_typ_inf.Pointer.child != ZonFieldResult) // Target slice is contains primitive type elements (u8),
                    return ZonGetFieldsError.BadReportType;         // should not have happened.

                if (@typeInfo(ptr_tgt_slc_typ_inf.Pointer.child).Pointer.is_const)
                {
                    // Set the slice `tgt` is a pointer to,
                    // to field's value string within `ast.source`.
                    // Note that `ast.source` is [:0]const u8, meaning the target slice elements
                    // must be const u8
                    ptr_tgt_slc.* = strv;
                    ptr_report.* = ZonFieldResult.filled;
                    return; // Done with this field
                }
                // Target slice elements are non-const u8, try using allocr (if any) to clone the value
                // to a new buffer. Caller will own this buffer.
                if (allocr) |alcr|
                {
                    ptr_tgt_slc.* = try alcr.dupe(u8, strv);
                    ptr_report.* = ZonFieldResult.filled;
                    return;
                }
                //
                const ast_fld_name = ast.tokenSlice(ast.firstToken(arr_ast_fld_idx) - 2);
                std.log.warn("Value of field '{s}' is a string: {s}\n" ++
                             "target struct field is []u8, but no allocator provided to clone the string",
                             .{ ast_fld_name, strv });
                return ZonGetFieldsError.NoAllocator;
            }
            // Target slice elements are not u8's (and zon value is a string)
            const ast_fld_name = ast.tokenSlice(ast.firstToken(arr_ast_fld_idx) - 2);
            std.log.warn("Value of field '{s}' is a string = '{s}' " ++
                         "but target slice elements ({s}) are not u8 or const u8",
                         .{ ast_fld_name, strv, @typeName(tgt_slice_child_type) });
            return ZonGetFieldsError.IncompatibleTargetField;
        }
        // Value is not a string (no double quotation marks), it's an error
        const ast_fld_name = ast.tokenSlice(ast.firstToken(arr_ast_fld_idx) - 2);
        std.log.warn("Parsing of field '{s}' failed, or value is not an array: {s}\n" ++
                     "or string (no double quotation marks).",
                     .{ ast_fld_name, strv });
        return ZonGetFieldsError.PathElementNotArray;
    };
    // We have parsed AST element `arr_ast_fld_idx` as array.
    // Now allocate a new buffer for array elements and assign it to `tgt`.
    // Also, if target slice elements are not of a primitive type (integers, floats, etc.),
    // then we must allocate yet another slice for the report struct and return it via `ptr_report`.
    if (allocr) |alcr|
    {
        var tgt_buf = try alcr.alloc(tgt_slice_child_type, arr_init.ast.elements.len);
        switch (tgt_slc_chld_typ_inf)
        {
            .Int =>
            {
                for (arr_init.ast.elements, 0..) |ast_arr_elt_fld_idx, i|
                    tgt_buf[i] = try getValueInt(tgt_slice_child_type, ast, ast_arr_elt_fld_idx);
                ptr_report.* = ZonFieldResult.filled;
            },
            .Float =>
            {
                for (arr_init.ast.elements, 0..) |ast_arr_elt_fld_idx, i|
                    tgt_buf[i] = try getValueFloat(tgt_slice_child_type, ast, ast_arr_elt_fld_idx);
                ptr_report.* = ZonFieldResult.filled;
            },
            .Bool =>
            {
                for (arr_init.ast.elements, 0..) |ast_arr_elt_fld_idx, i|
                    tgt_buf[i] = try getValueBool(ast, ast_arr_elt_fld_idx);
                ptr_report.* = ZonFieldResult.filled;
            },
            .Struct =>
            {
                ptr_report.* = try alcr.alloc(@typeInfo(report_typ_inf.Pointer.child).Pointer.child,
                                              arr_init.ast.elements.len);
                for (arr_init.ast.elements, 0..) |ast_arr_elt_fld_idx, i|
                {
                    const struct_res = try populateStruct(&tgt_buf[i], ast, ast_arr_elt_fld_idx,
                                                          ZonFieldResult.filled, allocr, recursion_depth + 1);
                    (ptr_report.*)[i] = struct_res;
                }
            },
            .Array =>
            {
                ptr_report.* = try alcr.alloc(@typeInfo(report_typ_inf.Pointer.child).Pointer.child,
                                              arr_init.ast.elements.len);
                for (arr_init.ast.elements, 0..) |ast_arr_elt_fld_idx, i|
                {
                    const arr_res = try populateArray(&tgt_buf[i], ast, ast_arr_elt_fld_idx, allocr,
                                                      recursion_depth + 1);
                    (ptr_report.*)[i] = arr_res;
                }
            },
            .Pointer => |ptr_info|
            {
                // We don't support element types other than slices
                if (ptr_info.size == .Slice) // Target slice element is also slice
                {
                    // Allocate memory for the report slice.
                    // The caller (end user) will own this slice together with the rest of
                    // the report and target structs.
                    const ReptSliceEltType = MakeReportSliceType(report_typ_inf.Pointer.child);
                    var report_for_slice = try alcr.alloc(ReptSliceEltType, arr_init.ast.elements.len);

                    for (arr_init.ast.elements, 0..) |ast_arr_elt_fld_idx, i|
                        // Pass a pointer to this slice field to `populateSlice`
                        try populateSlice(&tgt_buf[i], ast, ast_arr_elt_fld_idx, allocr,
                                          &report_for_slice[i], recursion_depth + 1);
                    // ptr_report MUST be a pointer to a slice
                    ptr_report.* = report_for_slice;
                }
                else
                    @compileError("Slice elements of pointer type " ++
                                  @typeName(@TypeOf(tgt_slice_child_type)) ++
                                  " are not supported");
            },
            else => @compileError("Slices of " ++ @typeName(tgt_slice_child_type) ++
                                  " are not supported")
        }
        ptr_tgt_slc.* = tgt_buf;
        return; // We're done
    }

    const ast_fld_name = ast.tokenSlice(ast.firstToken(arr_ast_fld_idx) - 2);
    std.log.warn("Field '{s}' parsed as array, but but no allocator provided",
                 .{ ast_fld_name });
    return ZonGetFieldsError.NoAllocator;
}

/// Returns the value of the field in `ast` indexed with `idx`, converted to the integer type `T`,
/// or an error.
/// If the value string of the field starts and ends with double quotation marks, those are ignored.
/// If `T` is u8, and the value string starts and ends with single quotation marks ('), the string
/// is treated as a (8-bit) character.
/// `T`   Integer type to be returned.
/// `ast` Abstract syntax tree generated from the ZON source text.
/// `idx` Index of the field node in `ast`.
fn getValueInt(comptime T: type, ast: std.zig.Ast, idx: std.zig.Ast.Node.Index) !T
{
    if (@typeInfo(T) != .Int)
        @compileError("fn getValueInt only works with integer types, " ++ @typeName(T) ++
                      " was provided");
    var str_val = getValueSlice(ast, ast.nodes.items(.main_token)[idx]);
    // Remove quotation marks if found
    if (str_val.len > 1 and str_val[0] == '"' and str_val[str_val.len - 1] == '"')
    {
        str_val.ptr += 1;
        str_val.len -= 2;
    }
    // u8 and u21 are the integer types used to reresent characters, so we must first try
    // to find out if the value of the field in ZON is meant to be a character.
    // If target field type is u8, check that AST slice (ZON field value as a string)
    // is in a format that's not going to make `std.zig.parseCharLiteral()` crash.
    // If yes, try calling it.
    if ((T == u8 or T == u21) and str_val[0] == '\'' and str_val[str_val.len - 1] == '\'')
    {
        if (str_val.len == 3) { return str_val[1]; } // Just one byte between ' and '
        else
            if (str_val.len > 3 and str_val.len <= 6 and
                (str_val[1] == '\\' or str_val[1] == 0 or // Must check parseCharLiteral's input
                ((str_val.len == 6 and str_val[1] & 0b11111000 == 0b11110000) or // to make sure
                 (str_val.len == 5 and str_val[1] & 0b11110000 == 0b11100000) or // it doesn't crash
                 (str_val.len == 4 and str_val[1] & 0b11100000 == 0b11000000))))
            {
                const parsed_char = std.zig.parseCharLiteral(str_val);
                switch (parsed_char)
                {
                    .success =>
                    {
                        if (T == u8 and parsed_char.success > 255)
                        {
                            std.log.warn("Field '{s}': value {s} (0x{x}) can't fit the requested type u8",
                                        .{ ast.tokenSlice(ast.firstToken(idx) - 2), str_val, parsed_char.success });
                            return ZonGetFieldsError.BadCharValue;
                        }
                        return if (T == u8) @as(T, @intCast(parsed_char.success & 0x0000FF))
                               else @as(T, @intCast(parsed_char.success)); // T == u21
                    },
                    .failure => // we only get here when str_val[1] == 0, but I'm not sure how to achieve this
                    {
                        std.log.warn("Field '{s}': value {s} could not be parsed as a character : {}",
                                    .{ ast.tokenSlice(ast.firstToken(idx) - 2), str_val, parsed_char.failure });
                        return ZonGetFieldsError.BadCharValue;
                    }
                }
            }
    }
    // ZON field value doesn't look like a character, try parsing in as int
    //                                  v-- Autodetect the base, to allow hex, bin, octa, etc.
    return std.fmt.parseInt(T, str_val, 0) catch |err|
    {
        std.log.warn("Could not convert value of field '{s}' = '{s}' to type {s} : {}",
                    .{ ast.tokenSlice(ast.firstToken(idx) - 2), str_val, @typeName(T), err });
        return err;
    };
}

/// Returns the value of the field in `ast` indexed with `idx`, converted to the floating-point
/// type `T`, or an error.
/// If the value string of the field starts and ends with double quotation marks, those are ignored.
/// `T`   Floating-point type to be returned.
/// `ast` Abstract syntax tree generated from the ZON source text.
/// `idx` Index of the field node in `ast`.
fn getValueFloat(comptime T: type, ast: std.zig.Ast, idx: std.zig.Ast.Node.Index) !T
{
    if (@typeInfo(T) != .Float)
        @compileError("fn getValueFloat only works with float types, " ++ @typeName(T) ++
                      " was provided");
    var str_val = getValueSlice(ast, ast.nodes.items(.main_token)[idx]);
    // Remove quotation marks if found
    if (str_val.len > 1 and str_val[0] == '"' and str_val[str_val.len - 1] == '"')
    {
        str_val.ptr += 1;
        str_val.len -= 2;
    }
    return std.fmt.parseFloat(T, str_val) catch |err|
    {
        std.log.warn("Could not convert value of field '{s}' = '{s}' to type {s} : {}",
                    .{ ast.tokenSlice(ast.firstToken(idx) - 2), str_val, @typeName(T), err });
        return err;
    };
}

/// Returns the value of the field in `ast` indexed with `idx`, converted to the bool type, or an error.
/// If the value string of the field starts and ends with double quotation marks, those are ignored.
/// `ast` Abstract syntax tree generated from the ZON source text.
/// `idx` Index of the field node in `ast`.
fn getValueBool(ast: std.zig.Ast, idx: std.zig.Ast.Node.Index) !bool
{
    var str_val = getValueSlice(ast, ast.nodes.items(.main_token)[idx]);
    // Remove quotation marks if found
    if (str_val.len > 1 and str_val[0] == '"' and str_val[str_val.len - 1] == '"')
    {
        str_val.ptr += 1;
        str_val.len -= 2;
    }
    return if (std.mem.eql(u8, str_val, "true")) true
           else
               if (std.mem.eql(u8, str_val, "false")) false
               else
               {
                   std.log.warn("Value of field '{s}' = '{s}' is neither 'true' nor 'false', " ++
                                "could not convert to bool",
                               .{ ast.tokenSlice(ast.firstToken(idx) - 2), str_val });
                   return ZonGetFieldsError.BadBooleanValue;
               };
}

// ---------------------------------------------------------
// ------------------- zonToStruct Tests -------------------
// ---------------------------------------------------------

test "zonToStruct big test"
{
    // Field order is not important
    const zon_txt =
        \\.{
        \\    .hex_u8 = 0x11, // Hexadecimal is OK
        \\    .str_u16 = "1991", // Number in a string is OK, as long as it's valid
        \\    .bin_u8 = 0b10010110, // Binary is OK too
        \\    .negative_i16 = -1000,
        \\    .arr_u16 = .{ 2, 4, 6, 8, 10, 12, 14 },
        \\    .slice_u16 = .{ 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 },
        \\    .nested_1 =
        \\    .{
        \\        .a_u8 = 0xFF,
        \\        .b_f64 = 0.5,
        \\        .s_slice_of_const_u8 = "Hello",
        \\        .s_slice_of_u8 = "String will be cloned",
        \\        .nested_2 =
        \\        .{
        \\            .c_u32 = 123456,
        \\            .d_i32 = -123456
        \\        }
        \\    },
        \\    .slice_of_structs = .{ .{ .c_u32 = 0, .d_i32 = 1 }, .{ .c_u32 = 2, .d_i32 = 3 } },
        \\    .slice_of_arrays = .{ .{ 0, 1, 2, 3 }, .{ 0, 10, 20, 30 } },
        \\    .character_1 = 'A',
        \\    .character_escaped = '\'',
        \\    .character_newline = '\x0A', // aka '\n'
        \\    .character_unicode = '⚡',
        \\    .str_unicode = "こんにちは",
        \\    .float_f32 = 12.45,
        \\    .boolean = true,
        \\    .arr_bool = .{ true, false, true, false },
        \\    .arr_f32 = .{ 4.0, 3.0, 2.0, 1.0, 0.0, -1.0 }, // Target has only 4 elements, but it's OK
        \\    .arr_i8 = .{ 127, 127, },
        \\    .arr_struct = .{ .{ .x = 1, .y = 1.0, .z = -1, .s = "OK_1" }, .{ .x = 2, .y = 2.0, .z = -2, .s = "OK_2" } }, // Missing one struct here, will be not_filled
        \\    .arr_arr = .{ .{ 1, 2 }, .{ 3, 4, 255, 100, 101 } }, // Jagged array, but that's fine
        \\    .arr_str_filled = "Hello", // String matches array length perfectly
        \\    .arr_str_overflow = "Hello", // Array is shorter than this string
        \\    .arr_str_partial = "Hello", // Array is longer than this string
        \\}
        ;

    var ast = try std.zig.Ast.parse(std.testing.allocator, zon_txt, .zon);
    defer ast.deinit(std.testing.allocator);

    const TargetStruct = struct
    {
        const Nstd1 = struct
        {
            const Nstd2 = struct
            {
                c_u32: u32 = 64,
                d_i32: i32 = -10,
            };

            a_u8: u8 = 0,
            b_f64: f64 = 0.0,
            // = &.{} is short for s: []<something> = undefined; s.len = 0;
            s_slice_of_const_u8: []const u8 = &.{}, // Will point to string value in `ast.source`
            s_slice_of_u8: []u8 = &.{}, // Will point to a string cloned using allocator
            nested_2: Nstd2 = .{},
        };

        const ArrStruct = struct
        {
            x: u8,
            y: f32,
            z: i32,
            s: []const u8, // Will point to string value in `ast.source`
        };

        bin_u8: u8 = 0,
        hex_u8: u8 = 0,
        character_1: u8 = 0,
        character_escaped: u8 = 0,
        character_newline: u8 = 0,
        character_unicode: u21 = 0,
        str_unicode: []const u8 = &.{}, // Will point to string value in `ast.source`
        str_u16: u16 = 0,
        negative_i16: i16 = 0,
        float_f32: f32 = 0.0,
        boolean: bool = false,
        extra_field: u32 = 7777, // Is not going to be modified, no value in ZON
        arr_u16: [5]u16 = .{ 0, 0, 0, 0, 0 },
        slice_u16: []u16 = &.{}, // Will be allocated using the provided allocator
        arr_bool: [4]bool = .{ false, false, false, false },
        arr_f32: [4]f32 = .{ 0.1, 0.2, 0.3, 0.4 },
        arr_i8: [4]i8 = .{ -1, -2, -3, -4 },
        arr_struct: [3]ArrStruct = .{ .{ .x = 0, .y = 0.0, .z = 0, .s = &.{} },
                                      .{ .x = 0, .y = 0.0, .z = 0, .s = &.{} },
                                      .{ .x = 123, .y = 45.6, .z = 789, .s = &.{} } },
        arr_arr: [2][2]i8 = .{ .{ 0, 0 }, .{ 0, 0 } },
        nested_1: Nstd1 = .{},
        slice_of_structs: []Nstd1.Nstd2 = &.{}, // Will be allocated using the provided allocator, ditto report field
        slice_of_arrays: [][4]u32 = &.{}, // Will be allocated using the provided allocator, ditto report field
        arr_str_filled: [5]u8 = [_]u8 { '!' } ** 5,
        arr_str_overflow: [2]u8 = [_]u8 { '!' } ** 2,
        arr_str_partial: [10]u8 = [_]u8 { '!' } ** 10,
    };

    var tgt_struct = TargetStruct {};
    const res = try zonToStruct(&tgt_struct, ast, std.testing.allocator);
    defer std.testing.allocator.free(tgt_struct.nested_1.s_slice_of_u8); // Must free `tgt_struct.s_slice_of_u8`
    defer std.testing.allocator.free(tgt_struct.slice_u16);              // Must free `tgt_struct.slice_u16`
    defer std.testing.allocator.free(tgt_struct.slice_of_structs);       // Must free `tgt_struct.slice_of_structs`
    defer std.testing.allocator.free(res.slice_of_structs);              // Must free `res.slice_of_structs`, because elements of `tgt_struct.slice_of_structs`
                                                                         // are not of a primitive type (not integer, float, or booleans) and so
                                                                         // `res.slice_of_structs` is a slice too.
    defer std.testing.allocator.free(tgt_struct.slice_of_arrays);        // Must free `tgt_struct.slice_of_structs`
    defer std.testing.allocator.free(res.slice_of_arrays);               // Must free `res.slice_of_arrays`, because elements of `tgt_struct.slice_of_arrays`
                                                                         // are not of a primitive type (not integer, float, or booleans) and so
                                                                         // `res.slice_of_arrays` is a slice too.
    try std.testing.expectEqual(res.hex_u8, ZonFieldResult.filled);
    try std.testing.expectEqual(tgt_struct.hex_u8, 0x11);
    try std.testing.expectEqual(res.str_u16, ZonFieldResult.filled);
    try std.testing.expectEqual(tgt_struct.str_u16, 1991);
    try std.testing.expectEqual(res.str_u16, ZonFieldResult.filled);
    try std.testing.expectEqual(tgt_struct.bin_u8, 0b10010110);
    try std.testing.expectEqual(res.negative_i16, ZonFieldResult.filled);
    try std.testing.expectEqual(tgt_struct.negative_i16, -1000);
    try std.testing.expectEqual(res.float_f32, ZonFieldResult.filled);
    try std.testing.expectEqual(tgt_struct.float_f32, 12.45); // See https://float.exposed , try this value
    try std.testing.expectEqual(res.character_1, ZonFieldResult.filled);
    try std.testing.expectEqual(tgt_struct.character_1, 'A');
    try std.testing.expectEqual(res.character_escaped, ZonFieldResult.filled);
    try std.testing.expectEqual(tgt_struct.character_escaped, '\'');
    try std.testing.expectEqual(res.character_newline, ZonFieldResult.filled);
    try std.testing.expectEqual(tgt_struct.character_newline, '\x0A');
    try std.testing.expectEqual(res.character_unicode, ZonFieldResult.filled);
    try std.testing.expectEqual(tgt_struct.character_unicode, '⚡');
    try std.testing.expectEqual(res.str_unicode, ZonFieldResult.filled);
    try std.testing.expectEqualStrings(tgt_struct.str_unicode, "こんにちは");
    try std.testing.expectEqual(res.boolean, ZonFieldResult.filled);
    try std.testing.expect(tgt_struct.boolean);
    try std.testing.expectEqual(tgt_struct.extra_field, 7777); // Field is not modified,
    try std.testing.expectEqual(res.extra_field, ZonFieldResult.not_filled); // no value in ZON
    var val_u16: u16 = 2;
    for (tgt_struct.arr_u16, res.arr_u16) |e, r|
    {
        try std.testing.expectEqual(r, ZonFieldResult.overflow_filled);
        try std.testing.expectEqual(e, val_u16);
        val_u16 += 2;
    }
    try std.testing.expectEqual(res.slice_u16, ZonFieldResult.filled);
    val_u16 = 11;
    for (tgt_struct.slice_u16) |e|
    {
        val_u16 -= 1; // Must be careful not to cause overflow (or, rather, underflow)
        try std.testing.expectEqual(e, val_u16);
    }
    for (tgt_struct.arr_bool, 0..) |e, i|
    {
        try std.testing.expectEqual(res.arr_bool[i], ZonFieldResult.filled);
        try std.testing.expectEqual(e, i & 1 == 0);
    }
    try std.testing.expectEqual(res.arr_f32[0], ZonFieldResult.overflow_filled);
    try std.testing.expectEqual(tgt_struct.arr_f32[0], 4.0);
    try std.testing.expectEqual(res.arr_f32[1], ZonFieldResult.overflow_filled);
    try std.testing.expectEqual(tgt_struct.arr_f32[1], 3.0);
    try std.testing.expectEqual(res.arr_f32[2], ZonFieldResult.overflow_filled);
    try std.testing.expectEqual(tgt_struct.arr_f32[2], 2.0);
    try std.testing.expectEqual(res.arr_f32[3], ZonFieldResult.overflow_filled);
    try std.testing.expectEqual(tgt_struct.arr_f32[3], 1.0);

    try std.testing.expectEqual(res.arr_i8[0], ZonFieldResult.partially_filled);
    try std.testing.expectEqual(tgt_struct.arr_i8[0], 127);
    try std.testing.expectEqual(res.arr_i8[1], ZonFieldResult.partially_filled);
    try std.testing.expectEqual(tgt_struct.arr_i8[1], 127);
    try std.testing.expectEqual(res.arr_i8[2], ZonFieldResult.not_filled);
    try std.testing.expectEqual(tgt_struct.arr_i8[2], -3);
    try std.testing.expectEqual(res.arr_i8[3], ZonFieldResult.not_filled);
    try std.testing.expectEqual(tgt_struct.arr_i8[3], -4);

    for (tgt_struct.arr_struct, res.arr_struct, 1..) |s, r, i|
    {
        if (i < 3)
        {
            // Scalar fields of structs that are array elemets, indicate whether that array
            // was filled, partially filled, i.e. there are unmofied elements at the end,
            // or overflow filled, i.e. there were more elements in AST than in the array.
            try std.testing.expectEqual(r.x, ZonFieldResult.partially_filled);
            try std.testing.expectEqual(s.x,   1 * i);
            try std.testing.expectEqual(r.y, ZonFieldResult.partially_filled);
            try std.testing.expectEqual(s.y, 1.0 * @as(f32, @floatFromInt(i)));
            try std.testing.expectEqual(r.z, ZonFieldResult.partially_filled);
            try std.testing.expectEqual(s.z,  -1 * @as(i32, @intCast(i)));
            // Array and slice fields are exempt from the rule above.
            try std.testing.expectEqual(r.s, ZonFieldResult.filled);
            var buf = [_]u8 { 0 } ** 4;
            _ = try std.fmt.bufPrint(&buf, "OK_{d:1}", .{i});
            try std.testing.expectEqualStrings(s.s, &buf);
        }
        else // We've left this element unmodified
        {
            try std.testing.expectEqual(r.x, ZonFieldResult.not_filled);
            try std.testing.expectEqual(s.x, 123);
            try std.testing.expectEqual(r.y, ZonFieldResult.not_filled);
            try std.testing.expectEqual(s.y, 45.6);
            try std.testing.expectEqual(r.z, ZonFieldResult.not_filled);
            try std.testing.expectEqual(s.z, 789);
            try std.testing.expectEqual(r.s, ZonFieldResult.not_filled);
            try std.testing.expectEqual(s.s.len, 0);
        }
    }
    try std.testing.expectEqual(res.nested_1.a_u8, ZonFieldResult.filled);
    try std.testing.expectEqual(tgt_struct.nested_1.a_u8, 0xFF);
    try std.testing.expectEqual(res.nested_1.b_f64, ZonFieldResult.filled);
    try std.testing.expectEqual(tgt_struct.nested_1.b_f64, 0.5);

    try std.testing.expectEqual(res.nested_1.s_slice_of_const_u8, ZonFieldResult.filled);
    try std.testing.expectEqualStrings(tgt_struct.nested_1.s_slice_of_const_u8, "Hello");
    // Since `tgt_struct.nested_1.s_slice_of_const_u8` is []const u8, it must be pointing at
    // the value string within `ast.source`
    try std.testing.expect
    (
        @intFromPtr(tgt_struct.nested_1.s_slice_of_const_u8.ptr) >= @intFromPtr(ast.source.ptr) and
        @intFromPtr(tgt_struct.nested_1.s_slice_of_const_u8.ptr) < @intFromPtr(ast.source.ptr) + ast.source.len
    );

    try std.testing.expectEqual(res.nested_1.s_slice_of_u8, ZonFieldResult.filled);
    try std.testing.expectEqualStrings(tgt_struct.nested_1.s_slice_of_u8, "String will be cloned");
    // Since `tgt_struct.nested_1.s_slice_of_u8` is []u8, it must be pointing at the cloned value string
    // ouside of `ast.source`
    try std.testing.expect
    (
        @intFromPtr(tgt_struct.nested_1.s_slice_of_u8.ptr) + tgt_struct.nested_1.s_slice_of_u8.len <
            @intFromPtr(ast.source.ptr) or
        @intFromPtr(tgt_struct.nested_1.s_slice_of_u8.ptr) > @intFromPtr(ast.source.ptr) + ast.source.len
    );

    var val_i8: i8 = 0;
    for (tgt_struct.arr_arr, res.arr_arr, 0..) |arr, res_arr, i|
        for (arr, res_arr) |e, r|
        {
            try std.testing.expectEqual(r, if (i == 0) ZonFieldResult.filled else ZonFieldResult.overflow_filled);
            val_i8 += 1;
            try std.testing.expectEqual(e, val_i8);
        };
    try std.testing.expectEqual(res.nested_1.nested_2.c_u32, ZonFieldResult.filled);
    try std.testing.expectEqual(tgt_struct.nested_1.nested_2.c_u32, 123456);
    try std.testing.expectEqual(res.nested_1.nested_2.d_i32, ZonFieldResult.filled);
    try std.testing.expectEqual(tgt_struct.nested_1.nested_2.d_i32, -123456);

    for (res.slice_of_structs) |res_elt|
    {
        try std.testing.expectEqual(res_elt.c_u32, ZonFieldResult.filled);
        try std.testing.expectEqual(res_elt.d_i32, ZonFieldResult.filled);
    }

    val_u16 = 0; // Neither u32 nor i32, but that's OK
    for (tgt_struct.slice_of_structs) |n2|
    {
        try std.testing.expectEqual(n2.c_u32, val_u16);
        val_u16 += 1;
        try std.testing.expectEqual(n2.d_i32, val_u16);
        val_u16 += 1;
    }

    for (res.arr_str_filled) |r| try std.testing.expectEqual(r, ZonFieldResult.filled);
    try std.testing.expectEqualStrings(&tgt_struct.arr_str_filled, "Hello");
    for (res.arr_str_overflow) |r| try std.testing.expectEqual(r, ZonFieldResult.overflow_filled);
    try std.testing.expectEqualStrings(&tgt_struct.arr_str_overflow, "He");
    for (res.arr_str_partial) |r| try std.testing.expectEqual(r, ZonFieldResult.partially_filled);
    try std.testing.expectEqualStrings((&tgt_struct.arr_str_partial)[0..5], "Hello");
    for ((&tgt_struct.arr_str_partial)[5..]) |e| try std.testing.expectEqual(e, 0);
}

test "zonToStruct: Slice of structs"
{
    const zon_txt = ".{ .slc_of_structs = .{ .{ .a = 0, .b = 1, .arr = .{ 0.0, 0.0 } }, .{ .a = 2, .b = 3, .arr = .{ 1.0, 1.0 } }, .{ .a = 4, .b = 5 } } }";
    var ast = try std.zig.Ast.parse(std.testing.allocator, zon_txt, .zon);
    defer ast.deinit(std.testing.allocator);

    const TargetStruct = struct
    {
        const StructWithArray = struct
        {
            a: u32 = 64,
            b: i32 = -10,
            arr: [2]f32 = [_]f32 { 0.0 } ** 2,
        };

        slc_of_structs: []StructWithArray = &.{}, // Will be allocated using the provided allocator, both here and in `report`
    };
    var tgt_struct = TargetStruct{};
    const report = try zonToStruct(&tgt_struct, ast, std.testing.allocator); // MUST pass allocator, since we have slices
    defer std.testing.allocator.free(tgt_struct.slc_of_structs); // Must free `tgt_struct.slc_of_structs`
    defer std.testing.allocator.free(report.slc_of_structs); // Must free `report.slc_of_structs`, since elements of `tgt_struct.slc_of_structs` are not primitive

    var i: i32 = 0;
    for (tgt_struct.slc_of_structs, 0..) |s, j| // Check `tgt_struct.slc_of_structs` values
    {
        try std.testing.expectEqual(s.a, @as(u32, @intCast(i)));
        i += 1;
        try std.testing.expectEqual(s.b, i);
        i += 1;
        // There's no value for the `arr` field of struct 2 in `zon_txt`
        if (j <= 1) for (s.arr) |e| try std.testing.expectEqual(e, @as(f32, @floatFromInt(j)) * 1.0);
    }
    for (report.slc_of_structs, 0..) |s, j| // Check the report
    {
        try std.testing.expectEqual(s.a, ZonFieldResult.filled);
        try std.testing.expectEqual(s.b, ZonFieldResult.filled);
        for (s.arr) |e| try std.testing.expectEqual(e, if (j <= 1) ZonFieldResult.filled else ZonFieldResult.not_filled);
    }
}

test "zonToStruct: Slice of arrays"
{
    const zon_txt = ".{ .slc_of_arrays = .{ .{ 10, 9, 8, }, .{ 7, 6, 5, }, .{ 4, 3, 2, }, .{ 1, 0 }, } }";
    var ast = try std.zig.Ast.parse(std.testing.allocator, zon_txt, .zon);
    defer ast.deinit(std.testing.allocator);

    const TargetStruct = struct
    {
        // Slice of 3-element u16 arrays
        slc_of_arrays: [][3]u16 = &.{}, // Will be allocated using the provided allocator, both here and in `report`
        slc_of_f32: []f32 = &.{}, // Will be allocated using the provided allocator, but only in the target struct
    };
    var tgt_struct = TargetStruct{};
    const report = try zonToStruct(&tgt_struct, ast, std.testing.allocator); // MUST pass allocator, since we have slices
    defer std.testing.allocator.free(tgt_struct.slc_of_arrays); // Must free `tgt_struct.slc_of_arrays`
    defer std.testing.allocator.free(report.slc_of_arrays);     // Must free `report.slc_of_arrays`, since elements of `tgt_struct.slc_of_arrays` are not primitive
    // `tgt_struct.slc_of_f32` is going to remain unallocated, but we know this only because the ZON
    // text is hardcoded. Normally you have to deallocate all slices in the target struct, and
    // their report counterparts if slice elements are not of elementary type (not integers, floats or booleans).
    defer std.testing.allocator.free(tgt_struct.slc_of_f32); // Must free `tgt_struct.slc_of_f32`. `report.slc_of_f32` is a `ZonFieldResult` enum though.

    var expected_val: i32 = 10;
    check_values: for (tgt_struct.slc_of_arrays) |s| // Check `tgt_struct.slc_of_arrays` values
    {
        for (s) |e|
        {
            try std.testing.expectEqual(e, expected_val);
            expected_val -= 1;
            if (expected_val < 0) // The last element of the last array is not there in `zon_txt`,
                break :check_values; // so we skip it. Normally you check your `report` to know that.
        }
    }
    for (report.slc_of_arrays, 0..) |s, i| // Check the report
    {
        for (s, 0..) |e, j| // Elements of the last array are going to be `.partially_filled`, then `.not_filled`
            try std.testing.expectEqual(e, if (i < 3) ZonFieldResult.filled else
                                               if (j < 2) ZonFieldResult.partially_filled else
                                                   ZonFieldResult.not_filled);
    }
    try std.testing.expectEqual(report.slc_of_f32, ZonFieldResult.not_filled);
}

test "zonToStruct: Slice of slices"
{
    const zon_txt = ".{ .slc_of_slices = .{ .{ 0, 0, 0, }, .{ 1, 1 }, .{ 2, }, .{ 3, 3 }, } }";
    var ast = try std.zig.Ast.parse(std.testing.allocator, zon_txt, .zon);
    defer ast.deinit(std.testing.allocator);

    const TargetStruct = struct
    {
        slc_of_slices: [][]u16 = &.{}, // Will be allocated using the provided allocator, both here and in `report`
    };
    var tgt_struct = TargetStruct{};
    const report = try zonToStruct(&tgt_struct, ast, std.testing.allocator); // MUST pass allocator, since we have slices
    defer
    {
        for (tgt_struct.slc_of_slices) |s| std.testing.allocator.free(s); // Free slice elements of `tgt_struct.slc_of_slices` FIRST
        std.testing.allocator.free(tgt_struct.slc_of_slices); // THEN free `tgt_struct.slc_of_slices` itself
        std.testing.allocator.free(report.slc_of_slices); // Also free `report.slc_of_slices`. Since elements of `tgt_struct.slc_of_slices` are not primitive, it's also a slice.
    }

    for (tgt_struct.slc_of_slices, 0..) |sl, i| // Check `tgt_struct.slc_of_slices` values
        for (sl) |e|
            try std.testing.expectEqual(e, i);
    for (report.slc_of_slices) |e| // Check the report
        try std.testing.expectEqual(e, ZonFieldResult.filled);
}

test "zonToStruct: NoAllocator error test"
{
    const zon_txt = ".{ .s_slice_of_u8 = \"Hello\" }";
    var ast = try std.zig.Ast.parse(std.testing.allocator, zon_txt, .zon);
    defer ast.deinit(std.testing.allocator);

    const TargetStruct = struct { s_slice_of_u8: []u8 = &.{}, };
    var tgt_struct = TargetStruct{};
    std.debug.print("\nDisregard the (warn) message below, it's expected and normal:\n", .{});
    try std.testing.expectError(error.NoAllocator, zonToStruct(&tgt_struct, ast, null)); // pass null instead of allocator
}

test "zonToStruct: PathElementNotStruct error test"
{
    const zon_txt = ".{ .my_struct = 12 } }";
    var ast = try std.zig.Ast.parse(std.testing.allocator, zon_txt, .zon);
    defer ast.deinit(std.testing.allocator);

    const TargetStruct = struct { const MyStruct = struct { ham: u8 = 0 }; my_struct: MyStruct = undefined, };
    var tgt_struct = TargetStruct{};
    std.debug.print("\nDisregard the (warn) message below, it's expected and normal:\n", .{});
    try std.testing.expectError(error.PathElementNotStruct, zonToStruct(&tgt_struct, ast, null));
}

test "zonToStruct: PathElementNotArray error test"
{
    const zon_txt = ".{ .my_arr = 12 } }";
    var ast = try std.zig.Ast.parse(std.testing.allocator, zon_txt, .zon);
    defer ast.deinit(std.testing.allocator);

    const TargetStruct = struct { my_arr: []u8 = undefined, };
    var tgt_struct = TargetStruct{};
    std.debug.print("\nDisregard the (warn) message below, it's expected and normal:\n", .{});
    try std.testing.expectError(error.PathElementNotArray, zonToStruct(&tgt_struct, ast, null));
}

test "zonToStruct: BadCharValue error test"
{
    const zon_txt = ".{ .my_char = '⚡' } }";
    var ast = try std.zig.Ast.parse(std.testing.allocator, zon_txt, .zon);
    defer ast.deinit(std.testing.allocator);

    const TargetStruct = struct { my_char: u8 = 0, };
    var tgt_struct = TargetStruct{};
    std.debug.print("\nDisregard the (warn) message below, it's expected and normal:\n", .{});
    try std.testing.expectError(error.BadCharValue, zonToStruct(&tgt_struct, ast, null));
}


test "zonToStruct: InvalidCharacter error test"
{
    const zon_txt = ".{ .my_char = A }"; // The character must be wrapped in single quotation marks
    var ast = try std.zig.Ast.parse(std.testing.allocator, zon_txt, .zon);
    defer ast.deinit(std.testing.allocator);
    //
    const TargetStruct = struct { my_char: u8 = 0, };
    var tgt_struct = TargetStruct{};
    std.debug.print("\nDisregard the (warn) messages below, they're expected and normal:\n", .{});
    try std.testing.expectError(error.InvalidCharacter, zonToStruct(&tgt_struct, ast, null));

    const zon_txt_2 = ".{ .my_f32 = 0.ABC }";
    var ast_2 = try std.zig.Ast.parse(std.testing.allocator, zon_txt_2, .zon);
    defer ast_2.deinit(std.testing.allocator);
    //
    const TargetStruct2 = struct { my_f32: f32 = 0.0, };
    var tgt_struct_2 = TargetStruct2{};
    try std.testing.expectError(error.InvalidCharacter, zonToStruct(&tgt_struct_2, ast_2, null));
}

test "zonToStruct: Overflow error test"
{
    const zon_txt = ".{ .my_u8 = 1234 }"; // The character must be wrapped in single quotation marks
    var ast = try std.zig.Ast.parse(std.testing.allocator, zon_txt, .zon);
    defer ast.deinit(std.testing.allocator);

    const TargetStruct = struct { my_u8: u8 = 0, };
    var tgt_struct = TargetStruct{};
    std.debug.print("\nDisregard the (warn) messages below, they're expected and normal:\n", .{});
    try std.testing.expectError(error.Overflow, zonToStruct(&tgt_struct, ast, null));

    const zon_txt_2 = ".{ .my_u8 = -1234 }"; // The character must be wrapped in single quotation marks
    var ast_2 = try std.zig.Ast.parse(std.testing.allocator, zon_txt_2, .zon);
    defer ast_2.deinit(std.testing.allocator);
    try std.testing.expectError(error.Overflow, zonToStruct(&tgt_struct, ast_2, null));
}

test "zonToStruct: BadBooleanValue error test"
{
    const zon_txt = ".{ .my_bool = \"not sure\" }";
    var ast = try std.zig.Ast.parse(std.testing.allocator, zon_txt, .zon);
    defer ast.deinit(std.testing.allocator);

    const TargetStruct = struct { my_bool: bool = false, };
    var tgt_struct = TargetStruct{};
    std.debug.print("\nDisregard the (warn) message below, it's expected and normal:\n", .{});
    try std.testing.expectError(error.BadBooleanValue, zonToStruct(&tgt_struct, ast, null));
}

test "zonToStruct: IncompatibleTargetField error test"
{
    const zon_txt = ".{ .my_str = \"abc\" }";
    var ast = try std.zig.Ast.parse(std.testing.allocator, zon_txt, .zon);
    defer ast.deinit(std.testing.allocator);
    //
    const TargetStruct = struct { my_str: [3]i16 = undefined };
    var tgt_struct = TargetStruct{};
    std.debug.print("\nDisregard the (warn) messages below, they're expected and normal:\n", .{});
    try std.testing.expectError(error.IncompatibleTargetField, zonToStruct(&tgt_struct, ast, null));

    const TargetStruct2 = struct { my_str: []i16 = undefined };
    var tgt_struct_2 = TargetStruct2{};
    try std.testing.expectError(error.IncompatibleTargetField, zonToStruct(&tgt_struct_2, ast, std.testing.allocator));
}

// The test below is designed to make compilation fail. Uncomment to check that it does.

// test "zonToStruct: recusion depth limit @compileError test"
// {
//     const zon_txt = ".{ .recursion = .{ .depth = .{ .limit = .{ .check = .{ .five = .{ .six = .{ .seven = .{ .eight = .{ .nine = .{ .ten = .{ .l11 = .{ .l12 = .{ .l13 = .{ .l14 = .{ .l15 = .{ .l16 = .{ .l17 = .{ .l18 = .{ .l19 = .{ .l20 = .{ .l21 = \"TO O DEEP\" }}}}}}}}}}}}}}}}}}}} }";
//     var ast = try std.zig.Ast.parse(std.testing.allocator, zon_txt, .zon);
//     defer ast.deinit(std.testing.allocator);
//
//     const TargetStruct = struct { recursion: N1=undefined, const N1 = struct { depth: N2=undefined, const N2 = struct { limit: N3=undefined, const N3 = struct { check: N4=undefined, const N4 = struct { five: N5=undefined, const N5 = struct { six: N6=undefined, const N6 = struct { seven: N7=undefined, const N7 = struct { eight: N8=undefined, const N8 = struct { nine: N9=undefined, const N9 = struct { ten: N10=undefined, const N10 = struct { l11: N11=undefined, const N11 = struct { l12: N12=undefined, const N12 = struct { l13: N13=undefined, const N13 = struct { l14: N14=undefined, const N14 = struct { l15: N15=undefined, const N15 = struct { l16: N16=undefined, const N16 = struct { l17: N17=undefined, const N17 = struct { l18: N18=undefined, const N18 = struct { l19: N19=undefined, const N19 = struct { l20: N20=undefined, const N20 = struct { l21: []const u8 }; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }; };
//     var tgt_struct = TargetStruct{};
//     _ = &tgt_struct;
//
//     // Assuming zon_fld_path_len_limit == 20
//     try std.testing.expect(zon_fld_path_len_limit <= 20);
//     // Otherwise this test makes no sense and must be adjusted accordingly
//     try zonToStruct(&tgt_struct, ast, null);
// }
