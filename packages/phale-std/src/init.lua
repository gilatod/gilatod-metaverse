local guard = require("meido.guard")
local pattern = require("meido.pattern")

local object = require("phale.object")
local typeclass = require("phale.typeclass")

local tablex = require("meido.tablex")

local unpack = table.unpack
local interpret = object.interpret

local std = {}
local _ENV = setmetatable({}, {__index = _G})

for k, v in pairs(pattern) do
    if getmetatable(v) == pattern then
        std[k] = v
        _ENV[k] = v
    end
end

local function chain(tag, ...)
    local args = {...}
    local exp = args[#args]
    for i = #args - 1, 1, -1 do
        exp = object(tag, args[i], exp)
    end
    return exp
end

-- first-class type

std.Type = typeclass("Type", {
    [typeclass] = CALLABLE:with_default(
        function(imp, itp, value) return value end),
    [pattern] = CALLABLE:with_default(
        function(imp, itp, value) return value end)
})

-- first-class function

std.Function = typeclass("Function", {
    ["function"] = CALLABLE:with_default(
        function(imp, itp, value) return value end),
    __call = CALLABLE:with_default(
        function(imp, itp, o, ...)
            return itp(itp(o)(imp, itp, ...)) end),
})

-- lambda

local function set_argument(itp, env, decl, value)
    if object.tag(decl) == "_" then
        local key = object.arguments(decl)[1]
        env[key] = value
    else
        local succ, info = pcall(itp(decl), env, value)
        if not succ then
            error("invalid argument: "..info)
        end
    end
end

local function clone_env(imp)
    local new_env = {}
    local env = imp.env
    if env then
        for k, v in pairs(env) do
            new_env[k] = v
        end
    end
    return new_env
end

local function modify_env(imp, env)
    return setmetatable({env = env}, {__index = imp})
end

std.Lambda = typeclass("Lambda", {
    lambda = CALLABLE:with_default(function(imp, itp, decl, body)
        return function(call_imp, call_itp, value, ...)
            local env = clone_env(imp)
            set_argument(itp, env, decl, call_itp(value))
            if select("#", ...) > 0 then
                return interpret(body, modify_env(imp, env))(call_imp, call_itp, ...)
            else
                return interpret(body, modify_env(imp, env))
            end
        end
    end),
    _ = CALLABLE:with_default(function(imp, itp, key)
        return imp.env[key]
    end)
}):inherit(std.Function)

local function match_lambda_argument(env, arg, arg_t)
    if getmetatable(arg_t) ~= typeclass then
        error("invalid typed lambda: invalid type")
    end
    local defaults = arg_t:get_defaults()
    if env then
        defaults = setmetatable({env = env}, {__index = defaults})
    end
    local succ, res = pcall(interpret, arg, defaults)
    if not succ then
        error(("failed to apply typed lambda: invalid argument (%s expected, got %s) \n  > %s")
            :format(arg_t, pattern.from_instance(arg) or type(arg), res))
    end
    return res
end

std.match_lambda_argument = match_lambda_argument

std.TypedLambda = typeclass("TypedLambda", {
    Type = typeclass:nilable(),
    typed_lambda = CALLABLE:with_default(function(imp, itp, decl, body)
        if object.tag(decl) == "__pow" then
            local id, type = unpack(object.arguments(decl))
            return function(call_imp, call_itp, value, ...)
                local env = clone_env(imp)
                local Type = imp.Type
                local arg = match_lambda_argument(call_imp.env, value,
                    Type and interpret(type, Type:get_defaults()) or itp(type))
                set_argument(itp, env, id, arg)
                if select("#", ...) > 0 then
                    return interpret(body, modify_env(imp, env))(call_imp, call_itp, ...)
                else
                    return interpret(body, modify_env(imp, env))
                end
            end
        else
            return function(call_imp, call_itp, value, ...)
                local env = clone_env(imp)
                set_argument(itp, env, decl, call_itp(value))
                if select("#", ...) > 0 then
                    return interpret(body, modify_env(imp, env))(call_imp, call_itp, ...)
                else
                    return interpret(body, modify_env(imp, env))
                end
            end
        end
    end)
}):inherit(std.Lambda, std.Type)

std._ = setmetatable({}, {
    __index = function(self, key)
        local obj = object("_", key)
        rawset(self, key, obj)
        return obj
    end
})

local SIMPLE_LAMBDA_ARG = std._.__

local function rewrite_simple_lambda(obj)
    if obj == std._ then
        return SIMPLE_LAMBDA_ARG
    elseif getmetatable(obj) ~= object then
        return obj
    end

    local t = {}
    local args = object.arguments(obj)
    for i = 1, #args do
        t[i] = rewrite_simple_lambda(args[i])
    end
    return object(object.tag(obj), unpack(t))
end

std.lambda = function(...)
    local count = select("#", ...)
    if count == 1 then
        local body = ...
        return object("lambda",
            SIMPLE_LAMBDA_ARG, rewrite_simple_lambda(body))
    end

    local arguments = {...}
    local body = arguments[count]
    arguments[count] = nil

    local exp = body
    for i = #arguments, 1, -1 do
        local arg = arguments[i]
        local tag = object.tag(arg) == "__pow"
            and "typed_lambda" or "lambda"
        exp = object(tag, arg, exp)
    end
    return exp
end

local lambda = std.lambda
local _ = std._

-- FixedPoint

std.FixedPoint = typeclass("FixedPoint", {
    fix = CALLABLE:with_default(function(imp, itp, f)
        local rec_f
        rec_f = itp(f)(imp, itp, function(call_imp, call_itp, ...)
            return rec_f(call_imp, call_itp, ...)
        end)
        return rec_f
    end)
}):inherit(std.Function)

-- std.fix = lambda(_.f, object("fix", _.f))
std.fix = function(f)
    return object("fix", f)
end

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
            if itp(branch[1])(imp, itp, value) then
                return itp(branch[2])
            end
        end
        error("incomplite cond")
    end)
})

--[[
std.if_ = lambda(_.o, _.true_branch, _.false_branch,
    object("if_", _.o, _.true_branch, _.false_branch))]]

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
            local succ, res = pcall(itp, lambda(branch[1], branch[2]))
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

-- core language

std.Core = typeclass("Core")
    :inherit(std.TypedLambda, std.FixedPoint, std.Condition)

-- show

std.Show = typeclass("Show", {
    show = CALLABLE:with_default(
        function(imp, itp, o) return tostring(itp(o)) end)
})

std.show = lambda(_.o, object("show", _.o))

-- read

std.READ = typeclass("read", {
    read = CALLABLE
})

std.read = lambda(_.s, object("read", _.s))

-- eq

std.Eq = typeclass("Eq", {
    eq = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) == itp(o2) end),
})

std.eq = lambda(_.o1, _.o2, object("eq", _.o1, _.o2))

-- enum

std.Enum = typeclass("Enum", {
    pred = CALLABLE:with_default(
        function(imp, itp, o) return itp(std.to_enum(std.from_enum(o) - 1)) end),
    succ = CALLABLE:with_default(
        function(imp, itp, o) return itp(std.to_enum(std.from_enum(o) + 1)) end),

    to_enum = CALLABLE,
    from_enum = CALLABLE
})

std.pred = lambda(_.o, object("pred", _.o))
std.succ = lambda(_.o, object("succ", _.o))
std.to_enum = lambda(_.n, object("to_enum", _.n))
std.from_enum = lambda(_.o, object("from_enum", _.o))

-- bounded

std.Bounded = typeclass("Bounded", {
    max_bound = CALLABLE,
    min_bound = CALLABLE
})

std.max_bound = lambda(_.o, object("max_bound", _.o))
std.min_bound = lambda(_.o, object("min_bound", _.o))

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
        error("invalid number")
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

std.inverse = lambda(_.o, -_.o)

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

std.minverse = lambda(_.o, object("minverse", _.o))

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

std.le = lambda(_.o1, _.o2, object("le", _.o1, _.o2))
std.gt = lambda(_.o1, _.o2, object("gt", _.o1, _.o2))

std.StrictPoset = typeclass("StrictPoset", {
    lt = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) < itp(o2) end),
    ge = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) >= itp(o2) end)
})

std.lt = lambda(_.o1, _.o2, object("lt", _.o1, _.o2))
std.ge = lambda(_.o1, _.o2, object("ge", _.o1, _.o2))

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

std.meet = lambda(_.o1, _.o2, object("meet", _.o1, _.o2))

std.JoinSemilattice = typeclass("JoinSemilattice", {
    join = CALLABLE:with_default(function(imp, itp, o1, o2)
        o1 = itp(o1)
        o2 = itp(o2)
        return o1 >= o2 and o2 or o1
    end)
}):inherit(std.Poset)

std.join = lambda(_.o1, _.o2, object("join", _.o1, _.o2))

std.Lattice = typeclass("Lattice")
    :inherit(std.MeetSemilattice, std.JoinSemilattice)

std.BoundedLattice = typeclass("BoundedLattice")
    :inherit(std.Lattice, std.Bounded)

std.ComplementedLattice = typeclass("ComplementedLattice", {
    complement = CALLABLE
}):inherit(std.BoundedLattice)

std.complement = lambda(_.o, object("complement", _.o))

-- signed

std.Signed = typeclass("Signed", {
    abs = CALLABLE:with_default(
        function(imp, itp, o) return math.abs(itp(o)) end),
    signum = CALLABLE:with_default(
        function(imp, itp, o) return itp(o) <= itp(std.unit)
            and itp(-std.munit) or itp(std.munit) end)
}):inherit(std.Poset, std.Monoid, std.MulMonoid)

std.abs = lambda(_.o, object("abs", _.o))
std.signum = lambda(_.o, object("signum", _.o))

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
    
    and_ = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) and itp(o2) end),
    or_ = CALLABLE:with_default(
        function(imp, itp, o1, o2) return itp(o1) or itp(o2) end)
})

std.and_ = function(...) return chain("and_", ...) end
std.or_ = function(...) return chain("or_", ...) end

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

-- tuple

local tuple = setmetatable({}, {
    __call = function(self, name, ...)
        guard.string("name", name)
        return setmetatable({name, {...}}, self)
    end
})
tuple.__index = tuple

function tuple:to_typeclass()
    local tc = self.typeclass
    if not tc then
        tc = typeclass(self[1], {
            [self] = CALLABLE:with_default(
                function(imp, itp, v) return v end)
        })
        self.typeclass = tc
    end
    return tc
end

function tuple:__tostring()
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
        function(imp, itp, t) return t:to_typeclass() end),

    tuple_instance = CALLABLE:with_default(function(imp, itp, t, ...)
        local args = {...}
        t:guard(itp, args)
        return setmetatable(args, t)
    end),
}):inherit(std.Type)

std.T = setmetatable({}, {
    __index = function(self, name)
        return function(...)
            return tuple(name, ...)
        end
    end
})

-- table

local table_mt = {
    __call = function(self, diff)
        guard.table("diff", diff)
        return object("table_update", self, diff)
    end,
    __newindex = "readonly"
}

std.Table = typeclass("Table", {
    table = CALLABLE:with_default(function(imp, itp, raw)
        local mt = getmetatable(raw)
        if mt == nil then
            local t = {}
            for k, v in pairs(raw) do
                t[k] = v
            end
            return setmetatable(t, table_mt)
        elseif mt == table_mt then
            return raw
        end
        error("invalid table")
    end),

    __index = CALLABLE:with_default(
        function(imp, itp, t, k) return itp(itp(t)[k]) end),
    
    table_update = CALLABLE:with_default(function(imp, itp, t, diff)
        local new = {}
        for k, v in pairs(diff) do
            new[k] = v
        end
        for k, v in pairs(t) do
            if new[k] == nil then
                new[k] = v
            end
        end
        return setmetatable(new, table_mt)
    end)
})

std.new_table = setmetatable({}, table_mt)

return std