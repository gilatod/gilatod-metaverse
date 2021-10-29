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

local typeclass = {}

setmetatable(typeclass, {
    __index = pattern.meta("phale.typeclass", typeclass),
    __call = function(self, name, pats)
        guard.string("name", name)

        local parents = {}
        local children = {}
        local patterns = {}
        local defaults = setmetatable({}, {
            __index = function(t, k)
                for i = #parents, 1, -1 do
                    local v = parents[i].defaults[k]
                    if v ~= nil then return v end
                end
            end
        })

        local instance = {
            parents = parents,
            children = children,
            patterns = patterns,
            defaults = defaults
        }

        local full = true
        if pats then
            guard.table("patterns", pats)
            for key, pat in pairs(pats) do
                local pat = pattern.from(pat)
                if pat == nil then
                    error("invalid field: "..key)
                elseif pat:has_default() then
                    defaults[key] = pat:get_default()
                else
                    full = false
                end
                patterns[key] = pat
            end
        end
        instance.full = full

        instance.pattern = pattern(
            "phale.typeclass", NO_DEFAULT,
            function() return name end,
            function(v, c, s)
                for i = #children, 1, -1 do
                    if children[i]:match(v, c, s) then
                        return true
                    end
                end
                if instance.full then
                    local co = co_create(interpret)
                    local succ, res = co_resume(co, v, defaults)
                    if not succ then error(res, 0) end
                    if co_status(co) ~= "suspended" then
                        if c then c["@"] = res end
                        return true
                    end
                end
            end)

        function instance:has_default() return false end
        function instance:get_description() return self.pattern:get_description() end
        function instance:match(v, c, s) return self.pattern:match(v, c, s) end
        function instance:guard(n, v) return self.pattern:guard(n, v) end

        return setmetatable(instance, self)
    end
})
typeclass.__index = typeclass

typeclass.skip = co_yield

function typeclass:__tostring()
    return tostring(self.pattern)
end

function typeclass:to_pattern()
    return self.pattern
end

function typeclass:is_full()
    return self.full
end

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

local function update_full(tc)
    local defaults = tc.defaults
    tc.full = pcall(for_patterns, tc,
        function(key, pat)
            if not defaults[key] then error() end
        end)
    local children = tc.children
    for i = 1, #children do
        update_full(children[i])
    end
end

function typeclass:instantiate(name, arguments)
    guard.string("name", name)

    local child = typeclass(name)
    local defaults = child.defaults
    child.parents[1] = self

    if arguments then
        guard.table("arguments", arguments)
        for k, v in pairs(arguments) do
            local pat = find_pattern(self, k)
            if pat then
                if not pat:match(v) then
                    error(("failed to instantiate %s (%s : %s expected, got %s)")
                        :format(self, k, pat, type(v)), 2)
                end
                defaults[k] = v
            end
        end
    end

    for_patterns(self, function(key, pat)
        if not defaults[key] then
            error(("failed to instantiate %s (%s : %s expected)")
                :format(self, key, pat), 5)
        end
    end)

    local children = self.children
    children[#children+1] = child
    return child
end

function typeclass:inherit(...)
    local tcs = {...}
    local parents = self.parents

    for i = 1, #tcs do
        local tc = tcs[i]
        typeclass:guard("argument", tc)
        parents[#parents+1] = tc
        local children = tc.children
        children[#children+1] = self
    end

    update_full(self)
    return self
end

return typeclass