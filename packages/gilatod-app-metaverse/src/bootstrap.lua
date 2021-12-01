local guard = require("meido.guard")
local meta = require("meido.meta")
local tablex = require("meido.tablex")
local pattern = require("meido.pattern")

local typeclass = require("phale.typeclass")
local object = require("phale.object")
local effect = require("phale.effect")

local std = require("phale-std")
for k, v in pairs(std) do _G[k] = v end

local lua = require("phale-lua")

local function test_phale()
    local LuaExpression = typeclass("LuaExpression")
        :inherit(Core)
    
    local LuaTuple = typeclass("LuaTuple")
        :inherit(LuaExpression, Tuple)

    local LuaRecord = typeclass("LuaRecord")
        :inherit(LuaExpression, Record)
    
    local LuaTable = typeclass("LuaTable")
        :inherit(LuaExpression, Table)

    local LuaNumber = typeclass("LuaNumber")
        :inherit(LuaExpression, Real)
    
    local LuaBoolean = typeclass("LuaBoolean")
        :inherit(LuaExpression, Boolean)

    local Lua = typeclass("lua")
        :inherit(LuaNumber, LuaBoolean, LuaTuple, LuaRecord, LuaTable)

    local coll = {}

    local test =
        fix(lambda(_.rec ^ Function, _.n ^ LuaNumber,
            if_(lt(_.n, 5),
                1 + _.rec(_.n + 1),
                1)))

    Lua:match(test(0), coll)
    print(tablex.show(coll['@']))

    local func = lambda(_.n ^ LuaNumber,
        cond | eq(1, _.n) >> LuaNumber
             | eq(2, _.n) >> LuaTable
             | eq(3, _.n) >> record { a = LuaNumber, b = LuaNumber })

    local t = new_table {a = 1, b = 2}

    Lua:match(
        cases({a = 1})
            | lambda(_.list ^ func(3), true) >> 1
            | lambda(ge(_, 1)) >> 10, coll)

    print(tablex.show(coll['@']))
end

local function test_lua()
    local coll = {}

    --[[
    local test =
        fix(lambda(_.rec ^ Function, _.n ^ lua.Number,
            if_(lt(_.n, 5),
                1 + _.rec(_.n + 1),
                1)))[[]]

    local test = (cond | eq(1, 1) >> 1) + lift(1)
    lua.Language:match(test, coll)
    print(tablex.show(coll['@']))
end

local function load()
    test_phale()
end

coroutine.wrap(function()
    xpcall(load, function(err)
        print(err)
        print(debug.traceback())
    end)
end)()
