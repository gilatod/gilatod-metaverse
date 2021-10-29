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
    local obj = object("__index", self, k)
    rawset(self, k, obj)
    return obj
end

function object:__call(...)
    return object("__call", {self, ...})
end

local function register_uniary(mt_func, tag)
    object[mt_func] = function(arg)
        return object(tag, arg)
    end
end

register_uniary("__unm", "__unm")
register_uniary("__len", "__len")
register_uniary("__bnot", "__bnot")

local function register_binary(mt_func, tag)
    object[mt_func] = function(left, right)
        return object(tag, left, right)
    end
end

register_binary("__add", "__add")
register_binary("__sub", "__sub")
register_binary("__mul", "__mul")
register_binary("__div", "__div")
register_binary("__idiv", "__idiv")

register_binary("__pow", "__pow")
register_binary("__mod", "__mod")
register_binary("__concat", "__concat")
-- register_binary("__eq", "__eq")
-- register_binary("__lt", "__lt")
-- register_binary("__le", "__le")

register_binary("__band", "__band")
register_binary("__bor", "__bor")
register_binary("__bxor", "__bxor")

register_binary("__shl", "__shl")
register_binary("__shr", "__shr")

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
            if not lift then
                error("failed to interpret raw value with type '"..type(obj).."'")
            end
            return lift(interpreters, do_interpret, obj)
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