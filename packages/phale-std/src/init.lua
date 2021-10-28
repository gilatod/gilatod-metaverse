local guard = require("meido.guard")
local pattern = require("meido.pattern")

local effect = require("phale.effect")
local object = require("phale.object")
local typeclass = require("phale.typeclass")

local unpack = table.unpack
local declare = effect.declare
local interpret = object.interpret

local std = {}
local _ENV = setmetatable({}, {__index = _G})

for k, v in pairs(pattern) do
    if getmetatable(v) == pattern then
        std[k] = v
        _ENV[k] = v
    end
end

-- show

std.SHOW = typeclass("show", {
    show = CALLABLE:with_default(
        function(imp, itp, o) return tostring(itp(o)) end)
})

std.show = function(o)
    return object("show", o)
end

-- read

std.READ = typeclass("read", {
    read = CALLABLE
})

std.read = function(str)
    guard.string(str)
    return object("read", str)
end

-- eq

std.EQ = typeclass("eq", {
    __eq = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) == itp(o2) end),
})

-- enum

std.ENUM = typeclass("enum", {
    pred = CALLABLE:with_default(
        function(imp, itp, o) return to_enum(from_enum(o) - 1) end),
    succ = CALLABLE:with_default(
        function(imp, itp, o) return to_enum(from_enum(o) + 1) end),

    to_enum = CALLABLE,
    from_enum = CALLABLE
})

std.pred = function(o)
    return object("pred", o)
end

std.succ = function(o)
    return object("succ", o)
end

std.to_enum = function(num)
    guard.number(num)
    return object("to_enum", num)
end

std.from_enum = function(o)
    return object("from_enum", o)
end

-- bounded

std.BOUNDED = typeclass("bounded", {
    max_bound = CALLABLE,
    min_bound = CALLABLE
})

std.max_bound = function(o)
    return object("max_bound", o)
end

std.min_bound = function(o)
    return object("min_bound", o)
end

-- additive group

std.SEMIGROUP = typeclass("semigroup", {
    __add = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) + itp(o2) end)
})

std.MONOID = typeclass("monoid", {
    ["@"] = CALLABLE:with_default(function(itp, value)
        if value == 0 then
            return std.unit
        else
            typeclass.skip()
        end
    end),
    unit = CALLABLE:with_default(
        function(imp, itp) return 0 end)
}):inherit(std.SEMIGROUP)

std.unit = object("unit")

std.GROUP = typeclass("group", {
    __unm = CALLABLE:with_default(
        function(imp, itp, o) return -itp(o) end),
    __sub = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) - itp(o2) end)
}):inherit(std.MONOID)

std.inverse = function(o)
    return -o
end

-- multiplicative group

std.MUL_SEMIGROUP = typeclass("mul_semigroup", {
    __mul = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) * itp(o2) end)
})

std.MUL_MONOID = typeclass("mul_monoid", {
    ["@"] = CALLABLE:with_default(function(itp, value)
        if value == 1 then
            return std.munit
        else
            typeclass.skip()
        end
    end),
    munit = CALLABLE:with_default(
        function(imp, itp) return 1 end)
}):inherit(std.MUL_SEMIGROUP)

std.munit = object("munit")

std.MUL_GROUP = typeclass("mul_group", {
    minverse = CALLABLE:with_default(
        function(imp, itp, o) return 1 / itp(o) end),
    __div = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) / itp(o2) end)
}):inherit(std.MUL_MONOID)

std.minverse = function(o)
    return object("minverse", o)
end

-- ring

std.SEMIRING = typeclass("semiring", {
    ["@"] = CALLABLE:with_default(function(itp, value)
        if value == 0 then
            return std.unit
        elseif value == 1 then
            return std.munit
        else
            typeclass.skip()
        end
    end),
}):inherit(std.MONOID, std.MUL_MONOID)

std.RING = typeclass("ring")
    :inherit(std.SEMIRING, std.GROUP)

std.FIELD = typeclass("field")
    :inherit(std.RING, std.MUL_GROUP)

-- order

std.POSET = typeclass("poset", {
    __le = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) <= itp(o2) end),
})

std.STRICT_POSET = typeclass("strict_poset", {
    __lt = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) < itp(o2) end),
})

std.ORD = typeclass("ord")
    :inherit(std.EQ, std.POSET, std.STRICT_POSET)

-- lattice

std.MEET_SEMILATTICE = typeclass("meet_semilattice", {
    meet = CALLALE
}):inherit(std.POSET)

std.meet = function(o1, o2)
    return object("meet", o1, o2)
end

std.JOIN_SEMILATTICE = typeclass("join_semilattice", {
    join = CALLALE
}):inherit(std.POSET)

std.join = function(o1, o2)
    return object("join", o1, o2)
end

std.LATTICE = typeclass("lattice")
    :inherit(std.MEET_SEMILATTICE, std.JOIN_SEMILATTICE)

std.BOUNDED_LATTICE = typeclass("bounded_lattice")
    :inherit(std.LATTICE, std.BOUNDED)

std.COMPLEMENTED_LATTICE = typeclass("complemented_lattice", {
    complement = CALLABLE
}):inherit(std.BOUNDED_LATTICE)

std.complement = function(o)
    return object("complement", o)
end

-- signed

std.SIGNED = typeclass("signed", {
    abs = CALLABLE,
    signum = CALLABLE
})

std.abs = function(o)
    return object("abs", o)
end

std.signum = function(o)
    return object("signum", o)
end

-- number

std.NUMBER = typeclass("number", {
    ["@"] = CALLABLE:with_default(function(itp, value)
        if math.type(value) == "integer" then
            return std.from_integer(value)
        else
            typeclass.skip()
        end
    end),
    from_integer = CALLABLE,
}):inherit(std.RING, std.SIGNED, std.EQ)

std.from_integer = function(num)
    guard.integer("num", num)
    return object("from_integer", num)
end

std.REAL = typeclass("real", {
    to_rational = CALLABLE
}):inherit(std.NUMBER, std.ORD)

std.to_rational = function(o)
    return object("to_rational", o)
end

std.INTEGRAL = typeclass("integral", {
    __idiv = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) // itp(o2) end),
    __mod = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) % itp(o2) end),
    to_integer = CALLABLE
}):inherit(std.REAL, std.ENUM)

std.to_integer = function(o)
    return object("to_integer", o)
end

std.FRACTIONAL = typeclass("fractional", {
    ["@"] = CALLABLE:with_default(function(itp, value)
        if type(value) == "number" then
            return std.from_rational(value)
        else
            typeclass.skip()
        end
    end),
    from_rational = CALLABLE
}):inherit(std.NUM, std.FIELD)

std.from_rational = function(num)
    guard.number("num", num)
    return object("from_rational", num)
end

std.FLOATING = typeclass("floating", {
    __pow = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) ^ itp(o2) end),
    exp = CALLABLE, sqrt = CALLABLE, log = CALLABLE, log_base = CALLABLE,
    sin = CALLABLE, tan = CALLABLE, cos = CALLABLE, asin = CALLABLE,
    atan = CALLABLE, acos = CALLABLE, sinh = CALLABLE, tanh = CALLABLE,
    cosh = CALLABLE, asinh = CALLABLE, atanh = CALLABLE, acosh = CALLABLE,
})

std.REAL_FRAC = typeclass("real_frac", {
    proper_fraction = CALLABLE,
    truncate = CALLABLE,
    round = CALLABLE,
    ceiling = CALLABLE,
    floor = CALLABLE
}):inherit(std.REAL, std.FRACTIONAL)

std.REAL_FLOAT = typeclass("real_float", {
    float_range = CALLABLE,
    is_NaN = CALLABLE,
    is_infinite = CALLABLE,
    is_nagative_zero = CALLABLE,
    atan2 = CALLABLE
}):inherit(std.FLOATING, std.REAL_FRAC)

-- binary

std.BINARY = typeclass("binary", {
    __bnot = CALLABLE:with_default(
        function(imp, itp, o) return ~itp(o) end),
    __band = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) & itp(o2) end),
    __bor = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) | itp(o2) end),
    __bxor = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) ~ itp(o2) end),

    __shl = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) << itp(o2) end),
    __shr = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) >> itp(o2) end)
})

-- functional

std.FUNCTIONAL = typeclass("functional", {
    __call = CALLABLE:with_default(
        function(imp, itp, o, ...) return itp(o)(...) end),
})

-- higher order

std.HIGHER_ORDER = typeclass("higher_order", {
    lambda = CALLABLE:with_default(function(imp, itp, arguments, body)
        return function(...)
            local env = {}
            local values = {...}
            for i = 1, #arguments do
                env[arguments[i]] = values[i]
            end
            return interpret(body, setmetatable({
                _ = function(imp, itp, argument)
                    return env[argument]
                end
            }, {__index = imp}))
        end
    end),

    _ = CALLABLE:with_default(function(imp, itp, argument)
        return std._(argument)
    end)
}):inherit(std.FUNCTIONAL)

std.lambda = function(...)
    local count = select("#", ...)
    local arguments = {...}
    local body = arguments[count]
    arguments[count] = nil
    return object("lambda", arguments, body)
end

std._ = function(argument)
    guard.non_nil("argument", argument)
    return object("_", argument, body)
end

-- condition

std.CONDITIONAL = typeclass("conditional", {
    cond = CALLABLE:with_default(function(imp, itp, o, true_condition, false_condition)
        if itp(o) then
            return itp(true_condition)
        else
            return itp(false_condition)
        end
    end)
}):inherit(std.HIGHER_ORDER)

std.cond = function(o, true_condition, false_condition)
    return object("cond", o, true_condition, false_condition)
end

-- guard

guard(asdf)
    | asdf >> 
    | asdf
    | jioasdf

local guard_checker_mt = {
    __call = function(self, case)
        local condition = case[1]
        return condition(self[0])
    end
}
guard
    | asdf | asdf | asdf | asdf

std.GUARD = typeclass("guard", {
    guard = CALLABLE:with_default(function(imp, itp, o)
        return setmetatable({itp(o)}, guard_checker_mt)
    end),

    __bor = CALLABLE:with_default(function(imp, itp, checker, case)
        if getmetatable(checker) ~= guard_checker_mt then
            typeclass.skip()
        elseif not checker.success and checker(itp(case)) then
            checker.success = true
        end
        return checker
    end),

    __shl = CALLABLE:with_default(function(imp, itp, condition, continuation)
        condition = itp(condition)
        if type(condition) ~= "function" then
            typeclass.skip()
        end

    end)
}):inherit(std.HIGHER_ORDER)

-- pattern matching

std.PATTERN_MATCHING = typeclass("pattern_matching", {
    caseof = CALLABLE:with_default(function(imp, itp, o)
        ~(caseof(people)
            | m(asdf) >> lambda
            | m(asef) >> jioasdf)
    end),
})

local case_mt = {}

local match_mt = {
    __shr = function(self, continuation)
        return setmetatable(
            {self[0], self[1], continuation}, case_mt)
    end
}

local caseof_mt
caseof_mt = {
    __bor = function(self, case)
        assert(getmetatable(case) == case_mt, "invalid case")
        local new = {unpack(self)}
        new[#new+1] = case
        return setmetatable(new, caseof_mt)
    end
}

std.caseof = function(o)
    return setmetatable({o})
end

return std