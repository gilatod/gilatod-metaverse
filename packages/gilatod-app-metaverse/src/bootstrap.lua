local guard = require("meido.guard")
local meta = require("meido.meta")
local tablex = require("meido.tablex")
local pattern = require("meido.pattern")

local class = require("phale.class")
local object = require("phale.object")
local effect = require("phale.effect")

local std = require("phale-std")
for k, v in pairs(std) do _G[k] = v end

local lua = require("phale-lua")

local function test_phale()
    local LuaExpression = class("LuaExpression")
        :inherit(Core, FixedPoint, ClassFamily)
    
    local LuaTuple = class("LuaTuple")
        :inherit(LuaExpression, FTuple)

    local LuaRecord = class("LuaRecord")
        :inherit(LuaExpression, FRecord)
    
    local LuaTable = class("LuaTable")
        :inherit(LuaExpression, Table)

    local LuaNumber = class("LuaNumber")
        :inherit(LuaExpression, Real)
    
    local LuaBoolean = class("LuaBoolean")
        :inherit(LuaExpression, Boolean)

    local Lua = class("lua")
        :inherit(LuaNumber, LuaBoolean, LuaTuple, LuaRecord, LuaTable)

    local function run(obj)
        local coll = {}
        Lua:match(obj, coll)
        print(tablex.show(coll['@']))
    end

    local test =
        fix(lambda(_.rec ^ Function, _.n ^ LuaNumber,
            if_(lt(_.n, 5),
                1 + _.rec(_.n + 1),
                1)))
    run(test(0))

    local func = lambda(_.n ^ LuaNumber,
        cases(_.n)
             | eq(1) >> LuaNumber
             | eq(2) >> LuaTable
             | eq(3) >> Tuple(LuaNumber, LuaNumber))

    local exp =
        cases({1, 2})
            | lambda(_.list ^ func(3), true) >> 1
            | otherwise >> 0
    run(exp)
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
