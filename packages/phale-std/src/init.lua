local guard = require("meido.guard")
local pattern = require("meido.pattern")

local object = require("phale.object")
local class = require("phale.class")

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

local function id(imp, itp, v) return v end
local function const(v) return function() return v end end

local function chain(tag, ...)
    local args = {...}
    local exp = args[#args]
    for i = #args - 1, 1, -1 do
        exp = object(tag, args[i], exp)
    end
    return exp
end

local function simple_class(name, key, default_method)
    if default_method then
        return class(name, {
            [key] = CALLABLE:with_default(default_method)
        })
    else
        return class(name, {
            [key] = CALLABLE
        })
    end
end

-- empty

std.Empty = class("Empty")

-- first-class class

std.Class = simple_class("Class", class, id)

-- variable class

std.VariableClass = simple_class("VariableClass",
    "var_class", function() return std.VariableClass end)
    :inherit(std.Class)

std.T = object("var_class")

-- class family

local class_family = setmetatable({}, {
    __call = function(self, name, ...)
        local instance = {
            name = name,
            argument_types = {...}
        }
        return setmetatable(instance, self)
    end
})
std.class_family = class_family
class_family.__index = class_family

function class_family:__tostring()
    return self.name
end

function class_family:get_argument_types()
    return self.argument_types
end

function class_family:var_arg(var_arg_type)
    self.var_arg_type = var_arg_type
    return self
end

function class_family:get_var_arg_type()
    return self.var_arg_type
end

local class_instance = {}
std.class_instance = class_instance
class_instance.__index = class_instance

function class_instance:get_family()
    return self[1]
end

function class_instance:get_arguments()
    return self[2]
end

function class_family:__call(...)
    local args = {...}
    return setmetatable({self, args}, class_instance)
end

std.ClassFamily = class("ClassFamily", {
    [class_family] = CALLABLE:with_default(id),
    [class_instance] = CALLABLE:with_default(function(imp, itp, instance)
        local family = instance:get_family()
        local family_interpreter = imp[family]
        if not family_interpreter then
            return Empty
        end

        local arg_types = family:get_argument_types()
        local args = instance:get_arguments()
        local interpreted = {}

        for i = 1, #arg_types do
            local arg_t = itp(arg_types[i])
            if getmetatable(arg_t) ~= class then
                error(("invalid class family %s: invalid type argument #%d")
                    :format(family, i))
            end

            local arg = args[i]
            local itps = arg_t:get_fields()
            local succ, res = pcall(interpret, arg, itps)
            if not succ then
                error(("failed to instantiate class family %s: invalid argument #%d (%s expected, got %s)\n\t> %s")
                    :format(family, i, arg_t, pattern.from_instance(arg) or type(arg), res))
            end

            interpreted[i] = res
        end

        local rest_count = #args - #arg_types
        if rest_count > 0 then
            local var_arg_t = family:get_var_arg_type()
            if var_arg_t then
                var_arg_t = itp(var_arg_t)
                if getmetatable(var_arg_t) ~= class then
                    error(("invalid class family %s: invalid type for variable arguments")
                        :format(family))
                end
                local itps = var_arg_t:get_fields()
                for i = rest_count, #args do
                    local arg = args[i]
                    local succ, res = pcall(interpret, arg, itps)
                    if not succ then
                        error(("failed to instantiate class family %s: invalid argument #%d (%s expected, got %s)\n\t> %s")
                            :format(family, i, var_arg_t, pattern.from_instance(arg) or type(arg), res))
                    end
                    interpreted[i] = res
                end
            else
                error("failed to instantiate class family %s: argument count does not match")
            end
        end

        return family_interpreter(imp, itp, unpack(interpreted))
    end)
}):inherit(std.VariableClass)

-- first-class function

std.Function = class("Function", {
    ["function"] = CALLABLE:with_default(id),
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

std.Lambda = class("Lambda", {
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
    if getmetatable(arg_t) ~= class then
        error("invalid typed lambda: invalid type")
    end
    local itps = arg_t:get_fields()
    if env then
        itps = setmetatable({env = env}, {__index = itps})
    end
    local succ, res = pcall(interpret, arg, itps)
    if not succ then
        error(("failed to apply typed lambda: invalid argument (%s expected, got %s)\n\t> %s")
            :format(arg_t, pattern.from_instance(arg) or type(arg), res))
    end
    return res
end

std.match_lambda_argument = match_lambda_argument

std.TypedLambda = class("TypedLambda", {
    Type = class:nilable(),
    typed_lambda = CALLABLE:with_default(function(imp, itp, decl, body)
        if object.tag(decl) == "__pow" then
            local id, type = unpack(object.arguments(decl))
            return function(call_imp, call_itp, value, ...)
                local env = clone_env(imp)
                local Type = imp.Type
                local arg = match_lambda_argument(call_imp.env, value,
                    Type and interpret(type, Type:get_fields()) or itp(type))
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
}):inherit(std.Lambda)

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

--[[ TODO: morphism

std.Morphism = class_family("Morphism", std.T, std.T)

std.SpecializedMorphism = class("SpecializedMorphism", {
    table = CALLABLE
})

std.FMorphism = class("Morphism", {
    Type = class:nilable(),
    [std.Morphism] = CALLABLE:with_default(function(imp, itp, source, target)

    end)
}):inherit(std.Function)
]]

-- fixed point

std.FixedPoint = class("FixedPoint", {
    fix = CALLABLE:with_default(function(imp, itp, f)
        local rec_f
        rec_f = itp(f)(imp, itp, function(call_imp, call_itp, ...)
            return rec_f(call_imp, call_itp, ...)
        end)
        return rec_f
    end)
}):inherit(std.Function)

std.fix = lambda(_.f, object("fix", _.f))

-- lift

std.Lift = class("Lift", {
    lift = CALLABLE:with_default(
        function(imp, itp, value) return itp(value) end)
})

std.lift = lambda(_.v, object("lift", _.v))

-- condition

local cases_mt = {
    __bor = function(self, branch)
        assert(object.tag(branch) == "__shr",
            "invalid branch for cases")
        local new = {unpack(self)}
        new[#new+1] = object.arguments(branch)
        return setmetatable(new, std.cases_mt)
    end
}
std.cases_mt = cases_mt

local cond_mt = {
    __bor = function(self, branch)
        assert(object.tag(branch) == "__shr",
            "invalid branch for cond")
        local new = {unpack(self)}
        new[#new+1] = object.arguments(branch)
        return setmetatable(new, std.cond_mt)
    end
}
std.cond_mt = cond_mt

std.Condition = class("Condition", {
    if_ = CALLABLE:with_default(function(imp, itp, o, true_branch, false_branch)
        if itp(o) then
            return itp(true_branch)
        else
            return itp(false_branch)
        end
    end),

    [cases_mt] = CALLABLE:with_default(function(imp, itp, c)
        local value = itp(c[1])
        for i = 2, #c do
            local branch = c[i]
            if itp(branch[1])(imp, itp, value) then
                return itp(branch[2])
            end
        end
    end),

    [cond_mt] = CALLABLE:with_default(function(imp, itp, c)
        for i = 1, #c do
            local branch = c[i]
            if itp(branch[1]) then
                return itp(branch[2])
            end
        end
    end)
})

std.if_ = function(o, true_branch, false_branch)
    return object("if_", o, true_branch, false_branch)
end

std.cases = function(o)
    return setmetatable({o}, cases_mt)
end

std.cond = setmetatable({}, cond_mt)

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

std.PatternMatching = class("PatternMatching", {
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

std.Core = class("Core")
    :inherit(std.Lift, std.TypedLambda, std.Condition)

-- show

std.Show = class("Show", {
    show = CALLABLE:with_default(
        function(imp, itp, o) return tostring(itp(o)) end)
})

std.show = lambda(_.o, object("show", _.o))

-- read

std.READ = class("read", {
    read = CALLABLE
})

std.read = lambda(_.s, object("read", _.s))

-- eq

std.Eq = class("Eq", {
    eq = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) == itp(b) end),
})

std.eq = lambda(_.a, _.b, object("eq", _.a, _.b))

-- enum

std.Enum = class("Enum", {
    pred = CALLABLE:with_default(
        function(imp, itp, o) return o - 1 end),
    succ = CALLABLE:with_default(
        function(imp, itp, o) return o + 1 end)
})

std.pred = lambda(_.o, object("pred", _.o))
std.succ = lambda(_.o, object("succ", _.o))

-- bounded

std.Bounded = class("Bounded", {
    max_bound = CALLABLE,
    min_bound = CALLABLE
})

std.max_bound = lambda(_.o, object("max_bound", _.o))
std.min_bound = lambda(_.o, object("min_bound", _.o))

-- additive group

std.Semigroup = class("Semigroup", {
    __add = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) + itp(b) end)
})

std.Monoid = class("Monoid", {
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

std.Group = class("Group", {
    __unm = CALLABLE:with_default(
        function(imp, itp, o) return -itp(o) end),
    __sub = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) - itp(b) end)
}):inherit(std.Monoid)

std.inverse = lambda(_.o, -_.o)

-- multiplicative group

std.MulSemigroup = class("MulSemigroup", {
    __mul = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) * itp(b) end)
})

std.MulMonoid = class("MulMonoid", {
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

std.MulGroup = class("MulGroup", {
    minverse = CALLABLE:with_default(
        function(imp, itp, o) return 1 / itp(o) end),
    __div = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) / itp(b) end)
}):inherit(std.MulMonoid)

std.minverse = lambda(_.o, object("minverse", _.o))

-- ring

std.Semiring = class("Semiring", {
    number = CALLABLE:with_default(function(imp, itp, value)
        if value == 0 then
            return itp(std.unit)
        elseif value == 1 then
            return itp(std.munit)
        end
        error("invalid semiring")
    end)
}):inherit(std.Monoid, std.MulMonoid)

std.Ring = class("Ring")
    :inherit(std.Semiring, std.Group)

std.Field = class("Field")
    :inherit(std.Ring, std.MulGroup)

-- order

std.Poset = class("Poset", {
    le = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) <= itp(b) end),
    gt = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) > itp(b) end)
})

std.le = lambda(_.a, _.b, object("le", _.a, _.b))
std.gt = lambda(_.a, _.b, object("gt", _.a, _.b))

std.StrictPoset = class("StrictPoset", {
    lt = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) < itp(b) end),
    ge = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) >= itp(b) end)
})

std.lt = lambda(_.a, _.b, object("lt", _.a, _.b))
std.ge = lambda(_.a, _.b, object("ge", _.a, _.b))

std.Ord = class("Ord")
    :inherit(std.Eq, std.Poset, std.StrictPoset)

-- lattice

std.MeetSemilattice = class("MeetSemilattice", {
    meet = CALLABLE:with_default(function(imp, itp, a, b)
        a = itp(a)
        b = itp(b)
        return a >= b and a or b
    end)
}):inherit(std.Poset)

std.meet = lambda(_.a, _.b, object("meet", _.a, _.b))

std.JoinSemilattice = class("JoinSemilattice", {
    join = CALLABLE:with_default(function(imp, itp, a, b)
        a = itp(a)
        b = itp(b)
        return a >= b and b or a
    end)
}):inherit(std.Poset)

std.join = lambda(_.a, _.b, object("join", _.a, _.b))

std.Lattice = class("Lattice")
    :inherit(std.MeetSemilattice, std.JoinSemilattice)

std.BoundedLattice = class("BoundedLattice")
    :inherit(std.Lattice, std.Bounded)

std.ComplementedLattice = class("ComplementedLattice", {
    complement = CALLABLE
}):inherit(std.BoundedLattice)

std.complement = lambda(_.o, object("complement", _.o))

-- signed

std.Signed = class("Signed", {
    abs = CALLABLE:with_default(
        function(imp, itp, o) return math.abs(itp(o)) end),
    signum = CALLABLE:with_default(
        function(imp, itp, o) return itp(o) <= itp(std.unit)
            and itp(-std.munit) or itp(std.munit) end)
}):inherit(std.Poset, std.Monoid, std.MulMonoid)

std.abs = lambda(_.o, object("abs", _.o))
std.signum = lambda(_.o, object("signum", _.o))

-- number

std.Number = class("Number", {
    number = CALLABLE:with_default(
        function(imp, itp, value) return value end)
}):inherit(std.Ring, std.Signed, std.Eq)

std.Real = class("Real")
    :inherit(std.Number, std.Ord)

std.Integral = class("Integral", {
    __idiv = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) // itp(b) end),
    __mod = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) % itp(b) end)
}):inherit(std.Real, std.Enum)

std.Fractional = class("Fractional")
    :inherit(std.Number, std.Field)

std.Floating = class("Floating", {
    __pow = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) ^ itp(b) end),
    exp = CALLABLE, sqrt = CALLABLE, log = CALLABLE, log_base = CALLABLE,
    sin = CALLABLE, tan = CALLABLE, cos = CALLABLE, asin = CALLABLE,
    atan = CALLABLE, acos = CALLABLE, sinh = CALLABLE, tanh = CALLABLE,
    cosh = CALLABLE, asinh = CALLABLE, atanh = CALLABLE, acosh = CALLABLE,
})

std.RealFrac = class("RealFrac", {
    proper_fraction = CALLABLE,
    truncate = CALLABLE,
    round = CALLABLE,
    ceiling = CALLABLE,
    floor = CALLABLE
}):inherit(std.Real, std.Fractional)

std.RealFloat = class("RealFloat", {
    float_range = CALLABLE,
    is_NaN = CALLABLE,
    is_infinite = CALLABLE,
    is_nagative_zero = CALLABLE,
    atan2 = CALLABLE
}):inherit(std.Floating, std.RealFrac)

-- boolean

std.Boolean = class("Boolean", {
    boolean = CALLABLE:with_default(
        function(imp, itp, value) return value end),
    
    and_ = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) and itp(b) end),
    or_ = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) or itp(b) end)
})

std.and_ = function(...) return chain("and_", ...) end
std.or_ = function(...) return chain("or_", ...) end

-- binary

std.Binary = class("Binary", {
    __bnot = CALLABLE:with_default(
        function(imp, itp, o) return ~itp(o) end),
    __band = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) & itp(b) end),
    __bor = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) | itp(b) end),
    __bxor = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) ~ itp(b) end),

    __shl = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) << itp(b) end),
    __shr = CALLABLE:with_default(
        function(imp, itp, a, b) return itp(a) >> itp(b) end)
})

-- string

std.String = simple_class("String", "string", id)

-- raw table

std.ReadonlyTable = class("RawTable", {
    table = CALLABLE:with_default(function(imp, itp, t)
        if getmetatable(t) then
            error("readonly table should not have metatable")
        end
        return t
    end),

    __index = CALLABLE:with_default(
        function(imp, itp, t, k) return itp(itp(t)[k]) end),
})

-- map

std.Map = class_family("Map", std.T, std.T)
std.SpecializedMap = class("SpecializedMap", {
    table = CALLABLE
})

std.FMap = simple_class("FMap", std.Map, function(imp, itp, key_t, value_t)
    local key_itps = key_t:get_fields()
    local value_itps = value_t:get_fields()
    return std.SpecializedMap:instantiate("MapInstance", {
        table = function(imp, itp, entries)
            local t = {}
            for k, v in pairs(entries) do
                local succ, res = pcall(interpret, k, key_itps)
                if not succ then
                    error(("invalid map entry: key has invalid type (%s expected, got %s)\n\t> %s")
                        :format(key_t, pattern.from_instance(k) or type(k), res))
                end
                k = res
                succ, res = pcall(interpret, v, value_itps)
                if not succ then
                    error(("invalid map entry '%s': value has invalid type (%s expected, got %s)\n\t> %s")
                        :format(k, value_t, pattern.from_instance(v) or type(v), res))
                end
                t[k] = v
            end
            return t
        end
    })
end):inherit(std.ClassFamily)

-- tuple

std.Tuple = class_family("Tuple")
    :var_arg(std.T)
std.SpecializedTuple = class("SpecializedTuple", {
    table = CALLABLE
})

std.FTuple = simple_class("Tuple", std.Tuple, function(imp, itp, ...)
    local arg_types = {...}
    return std.SpecializedTuple:instantiate("TupleInstance", {
        table = function(imp, itp, args)
            local t = {}
            for i = 1, #arg_types do
                local arg = args[i]
                local arg_t = arg_types[i]
                local itps = arg_t:get_fields()
                local succ, res = pcall(interpret, arg, itps)
                if not succ then
                    error(("invalid tuple component #%d (%s expected, got %s)\n\t> %s")
                        :format(i, arg_t, pattern.from_instance(arg) or type(arg), res))
                end
                t[i] = res
            end
            return t
        end
    })
end):inherit(std.ClassFamily)

-- record

std.Record = class_family("Record", std.Map(std.String, std.T))
std.SpecializedRecord = class("SpecializedRecord", {
    table = CALLABLE
})

std.FRecord = simple_class("Record", std.Record, function(imp, itp, entry_ts)
    return std.SpecializedRecord:instantiate("RecordInstance", {
        table = function(imp, itp, entries)
            local t = {}
            for k, entry_t in pairs(entry_ts) do
                local entry = entries[k]
                local itps = entry_t:get_fields()
                local succ, res = pcall(interpret, entry, itps)
                if not succ then
                    error(("invalid record entry '%s' (%s expected, got %s)\n\t> %s")
                        :format(k, entry_t, pattern.from_instance(entry) or type(entry), res))
                end
                t[k] = res
            end
            return t
        end
    })
end):inherit(std.FMap)

-- Array

std.Array = class_family("array", std.T)
std.SpecializedArray = class("SpecializedArray", {
    table = CALLABLE
})

std.FArray = simple_class("Array", std.Array, function(imp, itp, elem_t)
    return std.SpecializedArray:instantiate("ArrayInstance", {
        table = function(imp, itp, elems)
            local arr = {}
            local itps = elem_t:get_fields()
            for i = 1, #elems do
                local elem = elems[i]
                local succ, res = pcall(interpret, elem, itps)
                if not succ then
                    error(("invalid array element #%d (%s expected, got %s)\n\t> %s")
                        :format(i, elem_t, pattern.from_instance(elem) or type(elem), res))
                end
                arr[i] = res
            end
            return arr
        end
    })
end):inherit(std.ClassFamily)

-- table

local table_mt = {
    __call = function(self, diff)
        guard.table("diff", diff)
        return object("table_update", self, diff)
    end,
    __newindex = "readonly"
}

std.Table = class("Table", {
    [table_mt] = CALLABLE:with_default(
        function(imp, itp, t) return t end),
    
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
}):inherit(std.ReadonlyTable)

std.to_table = function(t)
    return setmetatable(t, table_mt)
end

std.new_table = setmetatable({}, table_mt)

return std