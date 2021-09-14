local meido = require "gilatod.meido"
local guard = meido.guard
local meta = meido.meta

local function load()
    print("HELLO WORLD")
end

coroutine.wrap(function()
    xpcall(load, function(err)
        print(err)
        print(debug.traceback())
    end)
end)()