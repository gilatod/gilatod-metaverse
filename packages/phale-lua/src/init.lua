local guard = require("meido.guard")
local pattern = require("meido.pattern")
local tablex = require("meido.tablex")

local object = require("phale.object")
local typeclass = require("phale.typeclass")

local std = require("phale-std")

local concat = table.concat
local unpack = table.unpack
local clone = tablex.clone
local interpret = object.interpret

local PAT_CONFIG = pattern.table {
}

local LuaCore = std.Core:instantiate("LuaCore", {
    -- Condition
    if_ = function(imp, itp, o, true_branch, false_branch)
        return "if "..itp(o).." then "..itp(true_branch).." else "..itp(false_branch) " end"
    end,
    cond = function(imp, itp, c)
        if #c <= 1 then return end
        local value = itp(c[1])
        for i = 2, #c do
            local branch = c[i]
            t[#t+1] = i == 2 and "if " or " elseif "
            t[#t+1] = itp(branch[1])(value)
            t[#t+1] = " then "
            t[#t+1] = itp(branch[2])
        end
        t[#t+1] = " end"
        return concat(t)
    end
})

local lua = setmetatable({}, {
    __call = function(self, config)
        PAT_CONFIG:guard("config", config)
        local language = LuaLanguage:instantiate("LuaLanguageInstance", {
            config = clone(config)
        })
        local instance = {language = language}
        return setmetatable(instance, self)
    end
})
lua.__index = lua

function lua:interpret(obj)
    local c = {}
    self.language:match(obj, c)
    return c["@"]
end

return lua