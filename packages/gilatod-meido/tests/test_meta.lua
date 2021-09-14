local meido = require "gilatod.meido"
local meta = meido.meta
local guard = meido.guard

local match_error = guard.match_error

section("readonly", function()
    match_error("protected metatable", function()
        local t = meta.readonly({})
        setmetatable(t, {})
    end)

    match_error("this table is read%-only", function()
        local t = meta.readonly({})
        t.a = 0
    end)
end)

section("writeonly", function()
    match_error("protected metatable", function()
        local t = meta.writeonly({}, function() end)
        setmetatable(t, {})
    end)

    match_error("this table is write%-only", function()
        local t = meta.writeonly({}, function() end)
        local b = t.b
    end)

    local written_t
    local written_k
    local written_v

    local t = meta.writeonly({}, function(t, k, v)
        written_t = t
        written_k = k
        written_v = v
    end)

    t.a = 2

    assert(written_t == t)
    assert(written_k == "a")
    assert(written_v == 2)
end)