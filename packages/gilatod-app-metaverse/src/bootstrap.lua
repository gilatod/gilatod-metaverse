local guard = require("meido.guard")
local meta = require("meido.meta")
local tablex = require("meido.tablex")
local pattern = require("meido.pattern")

local typeclass = require("phale.typeclass")
local object = require("phale.object")
local effect = require("phale.effect")

local std = require("phale-std")

local function load()
    local LUA_EXPRESSION = typeclass("lua_expression")
        :inherit(std.CONDITION, std.GUARD, std.TYPED_LAMBDA)
    local LUA_NUMBER = typeclass("lua_number")
        :inherit(std.REAL, LUA_EXPRESSION)
    local LUA = typeclass("lua")
        :inherit(LUA_NUMBER)

    local _ = std._
    local c = std.c
    local coll = {}
    LUA:match(
        ~(std.guard(1)
            | std.lambda(_.x ^ LUA_NUMBER, std.gt(_.x, 2)) >> 20
            | std.lambda(_.x ^ LUA_NUMBER, std.ge(_.x, 1)) >> 10), coll)
    print(coll['@'])

    LUA:match(
        std.cond(std.gt(1, 2), 1, 2), coll)
    print(coll['@'])
end

coroutine.wrap(function()
    xpcall(load, function(err)
        print(err)
        print(debug.traceback())
    end)
end)()