local guard = require("meido.guard")
local meta = require("meido.meta")
local tablex = require("meido.tablex")
local p = require("meido.pattern")

local object = require("phale.object")
local effect = require("phale.effect")

local prelude = require("phale-std")

local function load()
    print(tablex.show(prelude.replicate(effect("asdf"),
        object.memorize(function() return 2 end) + "1" ^ object.memorize(function() return 1 end))))
end

coroutine.wrap(function()
    xpcall(load, function(err)
        print(err)
        print(debug.traceback())
    end)
end)()