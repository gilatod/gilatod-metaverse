local guard = require("meido.guard")
local pattern = require("meido.pattern")

local class = require("phale.class")
local object = require("phale.object")
local std = require("phale.std")

local class_family = std.class_family
local lambda = std.lambda
local T = std.T
local _ = std._

local yield = coroutine.yield

local TAG_PURE = {}
local TAG_IMPURE = {}
local TAG_BIND = {}

local effect = {}

effect.EffectBase = class("EffectBase", {
    [effect_t] = CALLABLE:with_default(
        function(imp, itp, eff) return eff end),
    
    pure = CALLABLE:with_default(function(imp, itp, value)
        return setmetatable({TAG_PURE, res}, effect_t)
    end),

    impure = CALLABLE:with_default(function(imp, itp, tag))
        
    end
})

effect.Effect = class_family("Effect", T)
effect.SpecializedEffect = class("SpecializedEffect", {
    [effect_t] = CALLABLE:with_default(
        function(imp, itp, eff) return eff end),
    
    pure = CALLABLE:with_default(function(imp, itp, value)
        return setmetatable({TAG_PURE, res}, effect_t)
    end),

    bind = CALLABLE:with_default(function(imp, itp, eff, cont)
        eff = itp(eff)
        assert(getmetatable(eff) == effect, "invalid effect")
    end),

    inject = CALLABLE
})

local effect_t = {}

local FEffect = class("Effect", {
    impure = CALLABLE:with_default(function(imp, itp, tag_t)
        local tag_itps = tag_t:get_fields()
        return effect.SpecializedEffect:instantiate("EffectInstance", {
            inject = function(imp, itp, tag)
                local succ, res = pcall(interpret, tag, tag_itps)
                if not succ then
                    error(("invalid effect tag (%s expected, got %s)\n\t> %s")
                        :format(tag_t, pattern.from_instance(tag) or type(tag), res))
                end
                return setmetatable({res}, effect_t)
            end
        })
    end),
})

effect.inject = lambda(_.o, object("inject", _.o))
effect.bind = lambda(_.eff, _.cont, object("bind", _.eff, _.cont))

return effect