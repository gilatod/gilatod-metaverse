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
        :inherit(Condition, PatternMatching, TypedLambda)

    local LuaTuple = typeclass("LuaTuple")
        :inherit(LuaExpression, Tuple)

    local LuaNumber = typeclass("LuaNumber")
        :inherit(LuaExpression, Real)
    
    local LuaBoolean = typeclass("LuaBoolean")
        :inherit(LuaExpression, Boolean)

    local Lua = typeclass("lua")
        :inherit(LuaNumber, LuaBoolean, LuaTuple)

    local coll = {}

    local Test = T.Test(LuaNumber)
    local func = lambda(_.n ^ LuaNumber,
        if_(eq(_.n, 2), LuaNumber, Test))

    Lua:match(
        ~(cond(Test(1))
            | lambda(_.list ^ func(1), true) >> 20
            | lambda(_.x, ge(_.x, 1)) >> 10), coll)

    print(coll['@'])
end

coroutine.wrap(function()
    xpcall(load, function(err)
        print(err)
        print(debug.traceback())
    end)
end)()