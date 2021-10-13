local guard = require("meido.guard")
local p = require("meido.pattern")

local effect = require("phale.effect")
local object = require("phale.object")
local typeclass = require("phale.typeclass")

local declare = effect.declare

local prelude = {}

local FUNCTIONAL = typeclass("functional", {
    call = p.CALLABLE:with_default(
        function(imp, itp, o, ...) return itp(o)(...) end)
})

local MONOID = typeclass("monoid", {
    unit = p.CALLABLE,
    add = p.CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) + itp(o2) end)
})

local NUMBER = typeclass("number", {
    ["@"] = p.CALLABLE,

    unm = p.CALLABLE:with_default(
        function(imp, itp, o) return -itp(o) end),
    bnot = p.CALLABLE:with_default(
        function(imp, itp, o) return ~itp(o) end),

    sub = p.CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) - itp(o2) end),
    mul = p.CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) * itp(o2) end),
    div = p.CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) / itp(o2) end),
    idiv = p.CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) // itp(o2) end),

    pow = p.CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) ^ itp(o2) end),
    mod = p.CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) % itp(o2) end),
    eq = p.CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) == itp(o2) end),
    lt = p.CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) < itp(o2) end),
    le = p.CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) <= itp(o2) end),

    band = p.CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) & itp(o2) end),
    bor = p.CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) | itp(o2) end),
    bxor = p.CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) ~ itp(o2) end),

    shl = p.CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) << itp(o2) end),
    shr = p.CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) >> itp(o2) end)
}):inherit(FUNCTIONAL, MONOID)

local RAW_NUMBER = NUMBER:instantiate("raw_number", {
    ["@"] = function(interpret, value)
        if type(value) ~= "number" then
            typeclass.skip()
        end
        return value
    end
})

local STRANGE_NUMBER = RAW_NUMBER:instantiate("strange_number", {
    ["@"] = function(interpret, value)
        if type(value) ~= "number" then
            if value == "1" then
                value = 1
            else
                typeclass.skip()
            end
        end
        return value
    end
})

prelude.map = declare({"effect", effect}, {"mapper", p.CALLABLE})
prelude.append = declare({"effects", p.array(effect)})

prelude.chain = declare(
    {"effect", effect}, {"continuations", p.array(p.CALLABLE)})
prelude.replicate = declare(
    {"effect", effect}, {"count", NUMBER})

return prelude