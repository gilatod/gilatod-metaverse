local guard = require("meido.guard")
local pattern = require("meido.pattern")
local tablex = require("meido.tablex")

local object = require("phale.object")
local typeclass = require("phale.typeclass")

local std = require("phale-std")

local concat = table.concat
local unpack = table.unpack
local clone = tablex.clone
local interpret = object.interpret

local lua = {}

lua.Source = typeclass("LuaSource", {
    string = pattern.CALLABLE:with_default(
        function(imp, itp, s) return s end)
})

lua.Core = std.Core:instantiate("LuaCore", {
    -- Condition

    if_ = function(imp, itp, o, true_branch, false_branch)
        return "("..itp(o).." and "..itp(true_branch).." or "..itp(false_branch)..")"
    end,

    [cases_mt] = function(imp, itp, c)
        if #c <= 1 then return end
        local t = {"("}
        local value = itp(c[1])
        for i = 2, #c do
            local branch = c[i]
            t[#t+1] = itp(branch[1])(value)
            t[#t+1] = " and "
            t[#t+1] = itp(branch[2])
            t[#t+1] = " or ("
        end
        t[#t] = string.rep(")", #c - 1)
        return concat(t)
    end,

    [cond_mt] = function(imp, itp, c)
        if #c <= 0 then return end
        local t = {"("}
        for i = 1, #c do
            local branch = c[i]
            t[#t+1] = itp(branch[1])
            t[#t+1] = " and "
            t[#t+1] = itp(branch[2])
            t[#t+1] = " or ("
        end
        t[#t] = string.rep(")", #c)
        return concat(t)
    end
}):inherit(lua.Source)

lua.Eq = std.Eq:instantiate("LuaEq", {
    eq = function(imp, itp, a, b) return itp(a).."<="..itp(b) end,
})
lua.Show = std.Show:instantiate("LuaShow", {
    show = function(imp, itp, o) return "tostring("..itp(o)"..)" end,
})

lua.Number = std.Real:instantiate("LuaNumber", {
    number = function(imp, itp, value) return tostring(value) end,

    unit = function(imp, itp) return "0" end,
    munit = function(imp, itp) return "1" end,
    minverse = function(imp, itp, value) return "(1/("..itp(value).."))" end,

    pred = function(imp, itp, value) return "("..itp(value).."-1)" end,
    succ = function(imp, itp, value) return "("..itp(value).."+1)" end,

    abs = function(imp, itp, o) return "math.abs("..itp(o)..")" end,
    signum = function(imp, itp, o) return "("..itp(o).."<=0 and -1 or 1)" end,

    __unm = function(imp, itp, a) return "(-("..itp(a).."))" end,
    __add = function(imp, itp, a, b) return itp(a).."+"..itp(b) end,
    __sub = function(imp, itp, a, b) return itp(a).."-"..itp(b) end,
    __mul = function(imp, itp, a, b) return itp(a).."*"..itp(b) end,
    __div = function(imp, itp, a, b) return itp(a).."/"..itp(b) end,

    le = function(imp, itp, a, b) return itp(a).."<="..itp(b) end,
    gt = function(imp, itp, a, b) return itp(a)..">"..itp(b) end,
    lt = function(imp, itp, a, b) return itp(a).."<"..itp(b) end,
    ge = function(imp, itp, a, b) return itp(a)..">="..itp(b) end,
}):inherit(lua.Core, lua.Eq, lua.Show)

lua.Language = typeclass("LuaLanguage")
    :inherit(lua.Core, lua.Number)

return lua