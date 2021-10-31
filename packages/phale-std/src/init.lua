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
    eq = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) == itp(o2) end),
})

std.eq = function(o1, o2)
    return object("eq", o1, o2)
end

-- enum

std.ENUM = typeclass("enum", {
    pred = CALLABLE:with_default(
        function(imp, itp, o) return itp(std.to_enum(std.from_enum(o) - 1)) end),
    succ = CALLABLE:with_default(
        function(imp, itp, o) return itp(std.to_enum(std.from_enum(o) + 1)) end),

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
    number = CALLABLE:with_default(function(imp, itp, value)
        if value == 0 then
            return itp(std.unit)
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
    number = CALLABLE:with_default(function(imp, itp, value)
        if value == 1 then
            return itp(std.munit)
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
    number = CALLABLE:with_default(function(imp, itp, value)
        if value == 0 then
            return itp(std.unit)
        elseif value == 1 then
            return itp(std.munit)
        end
    end)
}):inherit(std.MONOID, std.MUL_MONOID)

std.RING = typeclass("ring")
    :inherit(std.SEMIRING, std.GROUP)

std.FIELD = typeclass("field")
    :inherit(std.RING, std.MUL_GROUP)

-- order

std.POSET = typeclass("poset", {
    le = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) <= itp(o2) end),
    gt = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) > itp(o2) end)
})

std.le = function(o1, o2)
    return object("le", o1, o2)
end

std.gt = function(o1, o2)
    return object("gt", o1, o2)
end

std.STRICT_POSET = typeclass("strict_poset", {
    ge = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) >= itp(o2) end),
    lt = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) < itp(o2) end)
})

std.ge = function(o1, o2)
    return object("ge", o1, o2)
end

std.lt = function(o1, o2)
    return object("lt", o1, o2)
end

std.ORD = typeclass("ord")
    :inherit(std.EQ, std.POSET, std.STRICT_POSET)

-- lattice

std.MEET_SEMILATTICE = typeclass("meet_semilattice", {
    meet = CALLABLE:with_default(function(imp, itp, o1, o2)
        o1 = itp(o1)
        o2 = itp(o2)
        return o1 >= o2 and o1 or o2
    end)
}):inherit(std.POSET)

std.meet = function(o1, o2)
    return object("meet", o1, o2)
end

std.JOIN_SEMILATTICE = typeclass("join_semilattice", {
    join = CALLABLE:with_default(function(imp, itp, o1, o2)
        o1 = itp(o1)
        o2 = itp(o2)
        return o1 >= o2 and o2 or o1
    end)
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
    abs = CALLABLE:with_default(
        function(imp, itp, o) return math.abs(itp(o)) end),
    signum = CALLABLE:with_default(
        function(imp, itp, o) return itp(o) >= 0 and 1 or -1 end)
})

std.abs = function(o)
    return object("abs", o)
end

std.signum = function(o)
    return object("signum", o)
end

-- number

std.NUMBER = typeclass("number", {
    number = CALLABLE:with_default(
        function(imp, itp, value) return value end)
}):inherit(std.RING, std.SIGNED, std.EQ)

std.REAL = typeclass("real")
    :inherit(std.NUMBER, std.ORD)

std.INTEGRAL = typeclass("integral", {
    __idiv = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) // itp(o2) end),
    __mod = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) % itp(o2) end)
}):inherit(std.REAL, std.ENUM)

std.FRACTIONAL = typeclass("fractional")
    :inherit(std.NUMBER, std.FIELD)

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

-- first-class type

std.FIRST_CLASS_TYPE = typeclass("frist_class_type", {
    [typeclass] = CALLABLE:with_default(
        function(imp, itp, value) return value end),
    [pattern] = CALLABLE:with_default(
        function(imp, itp, value) return value end)
})

-- constructor

std.constructed_mt = constructed_mt

std.CONSTRUCTOR = typeclass("constructor", {
    constructor = CALLABLE:with_default(function(imp, itp, name ...)
        local arg_types = {...}
        local constructor
        constructor = function(...)
            local args = {...}
            for i = 1, #arg_types do
                local arg_t = pattern.from(itp(arg_types[i]))
                if not arg_t then
                    error(("invalid constructor '%s': argument #%d has invalid type")
                        :format(name, i))
                end
                local arg = args[i]
                if not arg_t:match(arg) then
                    error(("failed to call constructor '%s': invalid argument #%d (%s expected, got %s)")
                        :format(name, i, arg_t, type(arg)))
                end
            end
            return setmetatable({constructor, args}, constructed_mt)
        end
        return constructor
    end)
}):inherit(std.FIRST_CLASS_TYPE)

std.C = setmetatable({}, {
    __index = function(self, name)
        return function(...)
            return object("constructor", name, ...)
        end
    end
})

-- algebric datatype

std.DATA = typeclass("data", {
    data = CALLABLE:with_default(function(imp, itp, )
        local MAYBE = data("maybe", _.just(NUMBER) | _.test(NUMBER, INT))
    end)
})

-- first-class function

std.FIRST_CLASS_FUNCTION = typeclass("first_class_function", {
    __call = CALLABLE:with_default(
        function(imp, itp, o, ...) return itp(o)(...) end),
})

-- lambda

local function set_argument(itp, env, arg, value)
    local succ, info = pcall(itp(arg), env, value)
    if not succ then
        error("invalid argument at #"..i..": "..info)
    end
end

std.LAMBDA = typeclass("lambda", {
    lambda = CALLABLE:with_default(function(imp, itp, arguments, body)
        return function(...)
            local values = {...}
            local env = {}
            for i = 1, #arguments do
                set_argument(itp, env, arguments[i], itp(values[i]))
            end
            return interpret(body, setmetatable({
                _ = function(sub_imp, sub_itp, argument)
                    return env[argument]
                end
            }, {__index = imp}))
        end
    end),

    _ = CALLABLE:with_default(function(imp, itp, key)
        return function(env, value)
            env[key] = value
        end
    end)
}):inherit(std.FIRST_CLASS_FUNCTION)

std.TYPED_LAMBDA = typeclass("typed_lambda", {
    typed_lambda = CALLABLE:with_default(function(imp, itp, arguments, body)
        return function(...)
            local values = {...}
            local env = {}

            for i = 1, #arguments do
                local arg = arguments[i]
                if object.tag(arg) == "__pow" then
                    local decl = object.arguments(arg)
                    local arg_t = pattern.from(itp(decl[2]))
                    if not arg_t then
                        error(("invalid typed lambda: argument #%d has invalid type")
                            :format(i))
                    end
                    local value = values[i]
                    local collec = {}
                    if not arg_t:match(value, collec) then
                        error(("failed to apply typed lambda: invalid argument #%d (%s expected, got %s)")
                            :format(i, arg_t, type(value)))
                    end
                    value = collec["@"] or value
                    set_argument(itp, env, decl[1], value)
                else
                    set_argument(itp, env, arg, itp(values[i]))
                end
            end

            return interpret(body, setmetatable({
                _ = function(sub_imp, sub_itp, argument)
                    return env[argument]
                end
            }, {__index = imp}))
        end
    end)
}):inherit(std.LAMBDA, std.FIRST_CLASS_TYPE)

std.lambda = function(...)
    local count = select("#", ...)
    local arguments = {...}
    local body = arguments[count]
    arguments[count] = nil

    local tag = "lambda"
    for i = 1, #arguments do
        local arg = arguments[i]
        if object.tag(arg) == "__pow" then
            tag = "typed_lambda"
            break
        end
    end
    return object(tag, arguments, body)
end

std._ = setmetatable({}, {
    __index = function(self, key)
        return object("_", key)
    end
})

-- condition

std.CONDITION = typeclass("condition", {
    cond = CALLABLE:with_default(function(imp, itp, o, true_condition, false_condition)
        if itp(o) then
            return itp(true_condition)
        else
            return itp(false_condition)
        end
    end)
})

std.cond = function(o, true_condition, false_condition)
    return object("cond", o, true_condition, false_condition)
end

-- guard

local guard_mt = {
    __bor = function(self, branch)
        assert(object.tag(branch) == "__shr",
            "invalid branch for guard")
        self[#self+1] = object.arguments(branch)
        return self
    end,
    __bnot = function(self)
        return object("guard", self)
    end
}

std.guard_mt = guard_mt

std.guard = function(o)
    return setmetatable({o}, guard_mt)
end

std.GUARD = typeclass("guard", {
    guard = CALLABLE:with_default(function(imp, itp, g)
        assert(getmetatable(g) == guard_mt, "invalid guard")
        local value = g[1]
        for i = 2, #g do
            local branch = g[i]
            if itp(branch[1])(value) then
                return itp(branch[2])
            end
        end
        error("incomplite guard")
    end)
}):inherit(std.FIRST_CLASS_FUNCTION)

-- pattern matching

local match_mt = {
    __bor = function(self, branch)
        assert(object.tag(branch) == "__shr",
            "invalid branch for match")
        self[#self+1] = object.arguments(branch)
        return self
    end,
    __bnot = function(self)
        return object("match", self)
    end
}

std.match_mt = match_mt

std.match = function(o)
    return setmetatable({o}, match_mt)
end

return std