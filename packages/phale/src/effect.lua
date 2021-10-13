local guard = require("meido.guard")
local pattern = require("meido.pattern")

local yield = coroutine.yield
local unpack = table.unpack

local effect = {}

setmetatable(effect, {
    __index = pattern.meta("phale.effect", effect),
    __call = function(self, tag, ...)
        guard.non_nil("tag", tag)
        return setmetatable({tag, ...}, self)
    end
})

effect.pure = function(object)
    return effect("$pure", object)
end

effect.bind = function(eff, continuation)
    return effect("$bind", eff, continuation)
end

effect.__concat = effect.bind

function effect:__call(...)
    return yield(setmetatable({...}, self));
end

effect.declare = function(...)
    local arg_specs = {...}
    for i = 1, #arg_specs do
        local spec = arg_specs[i]
        local name, pat = spec[1], spec[2]
        if type(name) ~= "string" then
            error(("invalid argument specification #%d: name must be string"):format(i), 2)
        elseif type(pat) ~= "table" or not pat.match then
            error(("invalid argument specification #%d '%s': pattern must have match function"):format(i, name), 2)
        end
    end
    local function create_effect(...)
        local args = {...}
        for i = 1, #arg_specs do
            local spec = arg_specs[i]
            local arg = args[i]
            local collection = {}
            if not spec[2]:match(arg, collection) then
                error(("invalid argument #%d '%s' (%s expected, got %s)"):format(i, spec[1], spec[2], type(arg)), 2)
            end
            local transformed = collection["@"]
            if transformed ~= nil then
                args[i] = transformed
            end
        end
        return effect(create_effect, unpack(args))
    end
    return create_effect
end

effect.handle = function(eff, handler)
    local tag = eff[1]
    if tag == "$pure" then
        return eff[2]
    elseif tag == "$bind" then
        local inner_eff, cont = eff[2], eff[3]
        local result = handle_effect(inner_eff, handler)
        if getmetatable(result) ~= effect then
            return handle_effect(cont(result), handler)
        else
            -- inject effect handler
            return effect.bind(result, function(inner_result)
                return handle_effect(cont(inner_result), handler)
            end)
        end
    else
        return handler(unpack(eff)) or eff
    end
end

return effect