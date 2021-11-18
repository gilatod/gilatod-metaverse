local guard = require("meido.guard")
local meta = require("meido.meta")
local tablex = require("meido.tablex")
local pattern = require("meido.pattern")

local typeclass = require("phale.typeclass")
local object = require("phale.object")
local effect = require("phale.effect")

local std = require("phale-std")
for k, v in pairs(std) do _G[k] = v end

local function load()
    local LuaExpression = typeclass("LuaExpression")
        :inherit(Core)
    
    local LuaTuple = typeclass("LuaTuple")
        :inherit(LuaExpression, Tuple)
    
    local LuaTable = typeclass("LuaTable")
        :inherit(LuaExpression, Table)

    local LuaNumber = typeclass("LuaNumber")
        :inherit(LuaExpression, Real)
    
    local LuaBoolean = typeclass("LuaBoolean")
        :inherit(LuaExpression, Boolean)

    local Lua = typeclass("lua")
        :inherit(LuaNumber, LuaBoolean, LuaTuple, LuaTable)

    local coll = {}

    local test =
        fix(lambda(_.rec ^ Function, _.n ^ LuaNumber,
            if_(lt(_.n, 5),
                1 + _.rec(_.n + 1),
                1)))

    Lua:match(test(0), coll)
    print(tablex.show(coll['@']))

    local Test = T.Test(LuaNumber)
    local func = lambda(_.n ^ LuaNumber,
        ~(cond(_.n)
            | eq(1) >> LuaNumber
            | eq(2) >> LuaTable
            | eq(3) >> Test))

    local t = new_table {a = 1, b = 2}

    Lua:match(
        ~(cond(Test(1))
            | lambda(_.list ^ func(3), true) >> t.a
            | lambda(ge(_, 1)) >> 10), coll)

    print(tablex.show(coll['@']))
end

coroutine.wrap(function()
    xpcall(load, function(err)
        print(err)
        print(debug.traceback())
    end)
end)()