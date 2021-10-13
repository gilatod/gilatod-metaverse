local meido = require "gilatod.meido"
local guard = meido.guard
local match_error = guard.match_error
guard.set_inspect_enabled(true)

section("relational guards", function()
    match_error("var is expected to be 0, got nil", function()
        guard.equal("var", nil, 0)
    end)

    match_error("var cannot be 0", function()
        guard.non_equal("var", 0, 0)
    end)

    match_error("var must be greater than 1, got 0", function()
        guard.greater("var", 0, 1)
    end)

    match_error("var must be greater or equal to 1, got 0", function()
        guard.greater_or_equal("var", 0, 1)
    end)

    guard.greater_or_equal("var", 1, 1)

    match_error("var must be less than 0, got 1", function()
        guard.less("var", 1, 0)
    end)

    match_error("var must be less or equal to 0, got 1", function()
        guard.less_or_equal("var", 1, 0)
    end)

    guard.less_or_equal("var", 2, 2)
end)

section("type guards", function()
    match_error("var must be string, got 0 %(number%)", function()
        guard.string("var", 0)
    end)
end)

section("nil guards", function()
    match_error("var must be nil, got 0", function()
        guard.is_nil("var", 0)
    end)

    match_error("var cannot be nil", function()
        guard.non_nil("var", nil)
    end)
end)

section("string gurads", function()
    match_error("var must be empty string, got \"hello\"", function()
        guard.empty_string("var", "hello")
    end)

    match_error("var must be non%-empty string", function()
        guard.nonempty_string("var", "")
    end)
end)

section("number guards", function()
    match_error("var must be zero, got 1", function()
        guard.zero("var", 1)
    end)

    match_error("var must be non%-zero", function()
        guard.non_zero("var", 0)
    end)

    match_error("var must be finite number", function()
        guard.finite("var", math.huge)
    end)

    match_error("var must be positive number, got %-1", function()
        guard.positive("var", -1)
    end)

    match_error("var must be zero or positive, got %-1", function()
        guard.zero_or_positive("var", -1)
    end)

    match_error("var must be negative number, got 1", function()
        guard.negative("var", 1)
    end)

    match_error("var must be zero or negative, got 1", function()
        guard.zero_or_negative("var", 1)
    end)

    match_error("var must be odd number, got 2", function()
        guard.odd("var", 2)
    end)

    match_error("var must be even number, got 1", function()
        guard.even("var", 1)
    end)
end)

section("table guards", function()
    match_error("var must be callable", function()
        local t = {}
        guard.callable("var", t)
    end)
    local callable = setmetatable({}, {
        __call = function() end
    })
    guard.callable("var", callable)
    guard.callable("var", function() end)
end)

section("boolean guards", function()
    match_error("var must be truthy, got nil", function()
        guard.truthy("var", nil)
    end)

    match_error("var must be falsy, got 0", function()
        guard.falsy("var", 0)
    end)

    match_error("var must be true, got 1", function()
        guard.is_true("var", 1)
    end)

    match_error("var must be false, got 0", function()
        guard.is_false("var", 0)
    end)
end)