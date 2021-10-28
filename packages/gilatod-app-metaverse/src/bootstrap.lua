local guard = require("meido.guard")
local meta = require("meido.meta")
local tablex = require("meido.tablex")
local p = require("meido.pattern")

local object = require("phale.object")
local effect = require("phale.effect")

local std = require("phale-std")

local function load()
end

coroutine.wrap(function()
    xpcall(load, function(err)
        print(err)
        print(debug.traceback())
    end)
end)()