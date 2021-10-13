local guard = require("meido.guard")
local pattern = require("meido.pattern")

local unpack = table.unpack

local object = {}

setmetatable(object, {
    __index = pattern.meta("phale.object", object),
    __call = function(self, tag, ...)
        guard.non_nil("tag", tag)
        return setmetatable({tag, {...}}, self)
    end
})

function object:__index(k)
    local obj = object("index", self, k)
    rawset(self, k, obj)
    return obj
end

function object:__call(...)
    return object("call", {self, ...})
end

local function register_uniary(mt_func, tag)
    object[mt_func] = function(arg)
        return object(tag, arg)
    end
end

register_uniary("__unm", "unm")
register_uniary("__len", "len")
register_uniary("__bnot", "bnot")

local function register_binary(mt_func, tag)
    object[mt_func] = function(left, right)
        return object(tag, left, right)
    end
end

register_binary("__add", "add")
register_binary("__sub", "sub")
register_binary("__mul", "mul")
register_binary("__div", "div")
register_binary("__idiv", "idiv")

register_binary("__pow", "pow")
register_binary("__mod", "mod")
register_binary("__concat", "concat")
register_binary("__eq", "eq")
register_binary("__lt", "lt")
register_binary("__le", "le")

register_binary("__band", "band")
register_binary("__bor", "bor")
register_binary("__bxor", "bxor")

register_binary("__shl", "shl")
register_binary("__shr", "shr")

object.memorize = function(computation)
    guard.callable("computation", computation)
    local value
    return object("memorize", function()
        if value == nil then
            value = computation()
        end
        return value
    end)
end

object.interpret = function(obj, interpreters)
    local function do_interpret(obj)
        if getmetatable(obj) ~= object then
            local lift = interpreters["@"]
            return lift and lift(do_interpret, obj) or obj
        end

        local tag = rawget(obj, 1)
        local args = rawget(obj, 2)

        if tag == "memorize" then
            return do_interpret(args[1]())
        end

        local interpreter = interpreters[tag]
        if not interpreter then
            error("failed to interpret object with tag '"..tag.."'")
        end
        return interpreter(interpreters, do_interpret, unpack(args))
    end
    return do_interpret(obj)
end

return object