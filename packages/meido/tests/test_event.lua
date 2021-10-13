local meido = require "gilatod.meido"
local event = meido.event

section("register & unregister", function()
    local t = {}
    local function func() end

    event.notify(t, "event", func)
    assert(t.event == func)

    event.notify(t, "event", func)
    assert(type(t.event) == "table")
    assert(t.event[1] == t.event[2] and t.event[1] == func)

    event.unnotify(t, "event", func)
    event.unnotify(t, "event", func)
    assert(t.event == nil)
end)

section("notify", function()
    local t = {}
    local func1_invoked
    local func2_invoked

    local function func1(sender, arg)
        assert(sender == t)
        assert(arg == "arg")
        func1_invoked = true
    end

    local function func2(sender, arg)
        assert(sender == t)
        assert(arg == "arg")
        func2_invoked = true
    end

    event.notify(t, "event", func1)
    event.notify(t, "event", func2)
    event.trigger(t, "event", "arg")

    assert(func1_invoked and func2_invoked)
end)