local js = require "js"
local jsenv = js.global

local meido = require "gilatod.meido"
local guard = meido.guard

local raw = require "assets_raw"

local coroutine_running = coroutine.running
local coroutine_yield = coroutine.yield
local coroutine_resume = coroutine.resume

local assets = {}

local function wrap(name)
    return function(path)
        guard.nonempty_string("path", path)

        local co = assert(coroutine_running(),
            "should be run in coroutine")

        raw[name](raw, path,
            function(res)
                coroutine_resume(co, res)
            end,
            function(reason)
                coroutine_resume(co, nil, reason)
            end)

        return coroutine_yield()
    end
end

assets.get_files = function()
    return raw:getFiles()
end

assets.load = wrap("load")
assets.load_module = wrap("loadModule")
assets.load_image = wrap("loadImage")

return assets