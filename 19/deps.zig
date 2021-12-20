const std = @import("std");
const Pkg = std.build.Pkg;
const string = []const u8;

pub const cache = ".zigmod/deps";

pub fn addAllTo(exe: *std.build.LibExeObjStep) void {
    @setEvalBranchQuota(1_000_000);
    for (packages) |pkg| {
        exe.addPackage(pkg.pkg.?);
    }
    inline for (std.meta.declarations(package_data)) |decl| {
        const pkg = @as(Package, @field(package_data, decl.name));
        var llc = false;
        inline for (pkg.system_libs) |item| {
            exe.linkSystemLibrary(item);
            llc = true;
        }
        inline for (pkg.c_include_dirs) |item| {
            exe.addIncludeDir(@field(dirs, decl.name) ++ "/" ++ item);
            llc = true;
        }
        inline for (pkg.c_source_files) |item| {
            exe.addCSourceFile(@field(dirs, decl.name) ++ "/" ++ item, pkg.c_source_flags);
            llc = true;
        }
        if (llc) {
            exe.linkLibC();
        }
    }
}

pub const Package = struct {
    directory: string,
    pkg: ?Pkg = null,
    c_include_dirs: []const string = &.{},
    c_source_files: []const string = &.{},
    c_source_flags: []const string = &.{},
    system_libs: []const string = &.{},
};

const dirs = struct {
    pub const _root = "";
    pub const _gvogwp4bjya6 = cache ++ "/../..";
    pub const _al1d3deiv60z = cache ++ "/git/github.com/ziglibs/zlm";
};

pub const package_data = struct {
    pub const _gvogwp4bjya6 = Package{
        .directory = dirs._gvogwp4bjya6,
    };
    pub const _al1d3deiv60z = Package{
        .directory = dirs._al1d3deiv60z,
        .pkg = Pkg{ .name = "zlm", .path = .{ .path = dirs._al1d3deiv60z ++ "/zlm.zig" }, .dependencies = null },
    };
    pub const _root = Package{
        .directory = dirs._root,
    };
};

pub const packages = &[_]Package{
    package_data._al1d3deiv60z,
};

pub const pkgs = struct {
    pub const zlm = package_data._al1d3deiv60z;
};

pub const imports = struct {
    pub const zlm = @import(".zigmod/deps/git/github.com/ziglibs/zlm/zlm.zig");
};
