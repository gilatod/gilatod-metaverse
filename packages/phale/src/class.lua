local guard = require("meido.guard")
local pattern = require("meido.pattern")

local object = require("phale.object")

local co_create = coroutine.create
local co_yield = coroutine.yield
local co_resume = coroutine.resume
local co_status = coroutine.status
local NO_DEFAULT = pattern.NO_DEFAULT
local collect = pattern.collect

local interpret = object.interpret

local class = {}

setmetatable(class, {
    __index = pattern.meta("phale.class", class),
    __tostring = function() return "%phale.class" end,
    __call = function(self, name, pats)
        guard.string("name", name)

        local parents = {}
        local children = {}
        local patterns = {}
        local fields = setmetatable({}, {
            __index = function(t, k)
                for i = #parents, 1, -1 do
                    local v = parents[i].fields[k]
                    if v ~= nil then return v end
                end
            end
        })
        local cache = {}

        local instance = {
            parents = parents,
            children = children,
            patterns = patterns,
            fields = fields,
            cache = cache
        }

        local complete = true
        if pats then
            guard.table("patterns", pats)
            for key, pat in pairs(pats) do
                local pat = pattern.from(pat)
                if pat == nil then
                    error(("invalid field for %s: %s")
                        :format(name, key))
                elseif pat:has_default() then
                    fields[key] = pat:get_default()
                else
                    complete = false
                end
                patterns[key] = pat
            end
        end
        instance.complete = complete

        instance.pattern = pattern(
            "phale.class", NO_DEFAULT,
            function() return name end,
            function(v, c, s)
                if not instance.complete then
                    error(("class %s is not complete"):format(name))
                end
                local res
                local entry = cache[v]
                if entry then
                    res = entry[1]
                else
                    res = interpret(v, fields)
                end
                if c then c["@"] = res end
                return true
            end)

        function instance:get_fields() return self.fields end
        function instance:is_complete() return self.complete end
        function instance:to_pattern() return self.pattern end
        function instance:has_default() return false end
        function instance:get_description() return self.pattern:get_description() end
        function instance:match(v, c, s) return self.pattern:match(v, c, s) end
        function instance:guard(n, v) return self.pattern:guard(n, v) end

        return setmetatable(instance, self)
    end
})
class.__index = class

function class:__tostring() return tostring(self.pattern) end

local function for_patterns(tc, f)
    local parents = tc.parents
    for i = 1, #parents do
        for_patterns(parents[i], f)
    end
    for key, pat in pairs(tc.patterns) do
        f(key, pat)
    end
end

local function find_pattern(tc, key)
    local pat = tc.patterns[key]
    if pat then return pat end
    
    local parents = tc.parents
    for i = 1, #parents do
        pat = find_pattern(parents[i], key)
        if pat then return pat end
    end
end

local function update_complete(tc)
    local fields = tc.fields
    tc.complete = pcall(for_patterns, tc,
        function(key, pat)
            if not fields[key] and not pat:match(nil) then
                error()
            end
        end)
    local children = tc.children
    for i = 1, #children do
        update_complete(children[i])
    end
end

function class:instantiate(name, arguments)
    guard.string("name", name)

    local child = class(name)
    child.parents[1] = self

    if arguments then
        child:provide(arguments)
    end

    local fields = child.fields
    for_patterns(self, function(key, pat)
        if not fields[key] and not pat:match(nil) then
            error(("failed to instantiate %s (%s : %s expected)")
                :format(self, key, pat), 5)
        end
    end)

    local children = self.children
    children[#children+1] = child
    return child
end

function class:provide(arguments)
    guard.table("arguments", arguments)
    local fields = self.fields
    for k, v in pairs(arguments) do
        local pat = find_pattern(self, k)
        if pat then
            if not pat:match(v) then
                error(("failed to instantiate %s (%s : %s expected, got %s)")
                    :format(self, k, pat, type(v)), 2)
            end
            fields[k] = v
        end
    end
end

function class:inherit(...)
    local tcs = {...}
    local parents = self.parents

    for i = 1, select("#", ...) do
        local tc = tcs[i]
        class:guard("argument", tc, 3)
        parents[#parents+1] = tc
        local children = tc.children
        children[#children+1] = self
    end

    update_complete(self)
    return self
end

function class:clear_cache()
    self.cache = {}
end

return class