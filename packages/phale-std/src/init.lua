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

std.Show = typeclass("Show", {
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

std.Eq = typeclass("Eq", {
    eq = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) == itp(o2) end),
})

std.eq = function(o1, o2)
    return object("eq", o1, o2)
end

-- enum

std.Enum = typeclass("Enum", {
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

std.Bounded = typeclass("Bounded", {
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

std.Semigroup = typeclass("Semigroup", {
    __add = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) + itp(o2) end)
})

std.Monoid = typeclass("Monoid", {
    number = CALLABLE:with_default(function(imp, itp, value)
        if value == 0 then
            return itp(std.unit)
        end
        error("invalid monoid")
    end),
    unit = CALLABLE:with_default(
        function(imp, itp) return 0 end)
}):inherit(std.Semigroup)

std.unit = object("unit")

std.Group = typeclass("Group", {
    __unm = CALLABLE:with_default(
        function(imp, itp, o) return -itp(o) end),
    __sub = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) - itp(o2) end)
}):inherit(std.Monoid)

std.inverse = function(o)
    return -o
end

-- multiplicative group

std.MulSemigroup = typeclass("MulSemigroup", {
    __mul = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) * itp(o2) end)
})

std.MulMonoid = typeclass("MulMonoid", {
    number = CALLABLE:with_default(function(imp, itp, value)
        if value == 1 then
            return itp(std.munit)
        end
        error("invalid multiplicative monoid")
    end),
    munit = CALLABLE:with_default(
        function(imp, itp) return 1 end)
}):inherit(std.MulSemigroup)

std.munit = object("munit")

std.MulGroup = typeclass("MulGroup", {
    minverse = CALLABLE:with_default(
        function(imp, itp, o) return 1 / itp(o) end),
    __div = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) / itp(o2) end)
}):inherit(std.MulMonoid)

std.minverse = function(o)
    return object("minverse", o)
end

-- ring

std.Semiring = typeclass("Semiring", {
    number = CALLABLE:with_default(function(imp, itp, value)
        if value == 0 then
            return itp(std.unit)
        elseif value == 1 then
            return itp(std.munit)
        end
        error("invalid semiring")
    end)
}):inherit(std.Monoid, std.MulMonoid)

std.Ring = typeclass("Ring")
    :inherit(std.Semiring, std.Group)

std.Field = typeclass("Field")
    :inherit(std.Ring, std.MulGroup)

-- order

std.Poset = typeclass("Poset", {
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

std.StrictPoset = typeclass("StrictPoset", {
    lt = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) < itp(o2) end),
    ge = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) >= itp(o2) end)
})

std.lt = function(o1, o2)
    return object("lt", o1, o2)
end

std.ge = function(o1, o2)
    return object("ge", o1, o2)
end

std.Ord = typeclass("Ord")
    :inherit(std.Eq, std.Poset, std.StrictPoset)

-- lattice

std.MeetSemilattice = typeclass("MeetSemilattice", {
    meet = CALLABLE:with_default(function(imp, itp, o1, o2)
        o1 = itp(o1)
        o2 = itp(o2)
        return o1 >= o2 and o1 or o2
    end)
}):inherit(std.Poset)

std.meet = function(o1, o2)
    return object("meet", o1, o2)
end

std.JoinSemilattice = typeclass("JoinSemilattice", {
    join = CALLABLE:with_default(function(imp, itp, o1, o2)
        o1 = itp(o1)
        o2 = itp(o2)
        return o1 >= o2 and o2 or o1
    end)
}):inherit(std.Poset)

std.join = function(o1, o2)
    return object("join", o1, o2)
end

std.Lattice = typeclass("Lattice")
    :inherit(std.MeetSemilattice, std.JoinSemilattice)

std.BoundedLattice = typeclass("BoundedLattice")
    :inherit(std.Lattice, std.Bounded)

std.ComplementedLattice = typeclass("ComplementedLattice", {
    complement = CALLABLE
}):inherit(std.BoundedLattice)

std.complement = function(o)
    return object("complement", o)
end

-- signed

std.Signed = typeclass("Signed", {
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

std.Number = typeclass("Number", {
    number = CALLABLE:with_default(
        function(imp, itp, value) return value end)
}):inherit(std.Ring, std.Signed, std.Eq)

std.Real = typeclass("Real")
    :inherit(std.Number, std.Ord)

std.Integral = typeclass("Integral", {
    __idiv = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) // itp(o2) end),
    __mod = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) % itp(o2) end)
}):inherit(std.Real, std.Enum)

std.Fractional = typeclass("Fractional")
    :inherit(std.Number, std.Field)

std.Floating = typeclass("Floating", {
    __pow = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) ^ itp(o2) end),
    exp = CALLABLE, sqrt = CALLABLE, log = CALLABLE, log_base = CALLABLE,
    sin = CALLABLE, tan = CALLABLE, cos = CALLABLE, asin = CALLABLE,
    atan = CALLABLE, acos = CALLABLE, sinh = CALLABLE, tanh = CALLABLE,
    cosh = CALLABLE, asinh = CALLABLE, atanh = CALLABLE, acosh = CALLABLE,
})

std.RealFrac = typeclass("RealFrac", {
    proper_fraction = CALLABLE,
    truncate = CALLABLE,
    round = CALLABLE,
    ceiling = CALLABLE,
    floor = CALLABLE
}):inherit(std.Real, std.Fractional)

std.RealFloat = typeclass("RealFloat", {
    float_range = CALLABLE,
    is_NaN = CALLABLE,
    is_infinite = CALLABLE,
    is_nagative_zero = CALLABLE,
    atan2 = CALLABLE
}):inherit(std.Floating, std.RealFrac)

-- boolean

std.Boolean = typeclass("Boolean", {
    boolean = CALLABLE:with_default(
        function(imp, itp, value) return value end),
    
    __and = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) and itp(o2) end),
    __or = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) or itp(o2) end)
})

std.__and = function(o1, o2)
    return object("__and", o1, o2)
end

std.__or = function(o1, o2)
    return object("__or", o1, o2)
end

-- binary

std.Binary = typeclass("Binary", {
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

std.Type = typeclass("Type", {
    [typeclass] = CALLABLE:with_default(
        function(imp, itp, value) return value end),
    [pattern] = CALLABLE:with_default(
        function(imp, itp, value) return value end)
})

-- tuple

local tuple = setmetatable({}, {
    __call = function(self, name, ...)
        guard.string("name", name)

        local tuple = {}
        tuple[1] = pattern.meta(name, tuple)
        tuple[2] = {...}

        return setmetatable(tuple, self)
    end
})
tuple.__index = tuple

function tuple:to_pattern()
    return self[1]
end

function tuple:__call(...)
    return object("tuple_instance", self, ...)
end

function tuple:guard(itp, args)
    local arg_types = self[2]
    for i = 2, #arg_types do
        local arg_t = pattern.from(itp(arg_types[i]))
        if not arg_t then
            error(("invalid tuple '%s': argument #%d has invalid type")
                :format(self.name, i))
        end
        local arg = args[i - 1]
        if not arg_t:match(arg) then
            error(("failed to instantiate tuple '%s': invalid argument #%d (%s expected, got %s)")
                :format(name, i, arg_t, pattern.from_instance(arg) or type(arg)))
        end
    end
end

std.tuple = tuple

std.Tuple = typeclass("Tuple", {
    [tuple] = CALLABLE:with_default(
        function(imp, itp, tuple) return tuple end),

    tuple_instance = CALLABLE:with_default(function(imp, itp, tuple, ...)
        local args = {...}
        tuple:guard(itp, args)
        return setmetatable(args, tuple)
    end),
}):inherit(std.Type)

std.T = setmetatable({}, {
    __index = function(self, name)
        return function(...)
            return tuple(name, ...)
        end
    end
})

-- algebric datatype

-- first-class function

std.Function = typeclass("Function", {
    __call = CALLABLE:with_default(
        function(imp, itp, o, ...) return itp(o)(...) end),
})

-- lambda

std.Lambda = typeclass("Lambda", {
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
}):inherit(std.Function)

local function match_lambda_argument(i, arg, arg_t)
    arg_t = pattern.from(arg_t)
    if not arg_t then
        error(("invalid typed lambda: argument #%d has invalid type")
            :format(i))
    end
    local collec = {}
    if not arg_t:match(arg, collec) then
        error(("failed to apply typed lambda: invalid argument #%d (%s expected, got %s)")
            :format(i, arg_t, pattern.from_instance(arg) or type(arg)))
    end
    return collec["@"] or arg
end

std.match_lambda_argument = match_lambda_argument

local function set_argument(itp, env, decl, value)
    local succ, info = pcall(itp(decl), env, value)
    if not succ then
        error("invalid argument at #"..i..": "..info)
    end
end

std.TypedLambda = typeclass("TypedLambda", {
    typed_lambda = CALLABLE:with_default(function(imp, itp, decls, body)
        return function(...)
            local values = {...}
            local env = {}

            for i = 1, #decls do
                local decl = decls[i]
                if object.tag(decl) == "__pow" then
                    decl = object.arguments(decl)
                    local arg = match_lambda_argument(i, values[i], itp(decl[2]))
                    set_argument(itp, env, decl[1], arg)
                else
                    set_argument(itp, env, decl, itp(values[i]))
                end
            end

            return interpret(body, setmetatable({
                _ = function(sub_imp, sub_itp, argument)
                    return env[argument]
                end
            }, {__index = imp}))
        end
    end)
}):inherit(std.Lambda, std.Type)

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

local cond_mt = {
    __bor = function(self, branch)
        assert(object.tag(branch) == "__shr",
            "invalid branch for cond")
        self[#self+1] = object.arguments(branch)
        return self
    end,
    __bnot = function(self)
        return object("cond", self)
    end
}

std.cond_mt = cond_mt

std.Condition = typeclass("Condition", {
    if_ = CALLABLE:with_default(function(imp, itp, o, true_branch, false_branch)
        if itp(o) then
            return itp(true_branch)
        else
            return itp(false_branch)
        end
    end),

    cond = CALLABLE:with_default(function(imp, itp, c)
        assert(getmetatable(c) == cond_mt, "invalid cond")
        local value = itp(c[1])
        for i = 2, #c do
            local branch = c[i]
            if itp(branch[1])(value) then
                return itp(branch[2])
            end
        end
        error("incomplite cond")
    end)
})

std.if_ = function(o, true_branch, false_branch)
    return object("if_", o, true_branch, false_branch)
end

std.cond = function(o)
    return setmetatable({o}, cond_mt)
end

-- patern matching

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

std.PatternMatching = typeclass("PatternMatching", {
    match = CALLABLE:with_default(function(imp, itp, m)
        assert(getmetatable(m) == match_mt, "invalid match")
        local value = itp(m[1])
        for i = 2, #m do
            local branch = m[i]
            local succ, res = pcall(itp, std.lambda(branch[1], branch[2]))
            if succ then
                return itp(branch[2])
            elseif not res:match("failed to apply typed lambda") then
                error(res, 0)
            end
        end
        error("incomplite guard")
    end)
}):inherit(std.Function)

std.match = function(o)
    return setmetatable({o}, match_mt)
end

return std