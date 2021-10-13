local guard = require("meido.guard")
local meta = require("meido.meta")
local pattern = require("meido.pattern")

local readonly = meta.readonly

local format = string.format

local struct = {}
struct.__index = struct

local CALLBACKS_KEY = readonly {}

local property_entry = setmetatable({}, {
    __call = function(self, t)
        return setmetatable(t, self)
    end
})
property_entry.__index = property_entry
property_entry.__metatable = "readonly"

function property_entry:get_type()
    return self[1]
end

function property_entry:guard(name, value)
    self[2](name, value)
end

function property_entry:get_default()
    return self[3]
end

local OPTIONAL_BYTE = string.byte('?')

local type_defaults = {
    ["string"]   = "",
    ["number"]   = 0,
    ["table"]    = pattern.TABLE:get_default(),
    ["function"] = pattern.FUNCTION:get_default(),
    ["boolean"]  = false,
    ["any"]      = false
}

local function register_raw_prop(prop_name, prop_type, prop_default)
    guard.nonempty_string("property name", prop_name)
    guard.nonempty_string("property type", prop_type)
    
    local optional

    if prop_type:byte(#prop_type) == OPTIONAL_BYTE then
        prop_type = prop_type:sub(1, #prop_type - 1)
        optional = true
    end

    local type_default = type_defaults[prop_type]
    if type_default == nil then
        error(format(
            "unrecognized type for '%s': %s",
            prop_name, prop_type), 3)
    end

    if optional then
        type_default = nil
    end

    if prop_default ~= nil then
        if type(prop_default) ~= prop_type then
            error(format(
                "invalid default value for '%s' (%s expected, got %s)",
                prop_name, prop_type, type(prop_default)), 3)
        end

        if prop_type == "table"
            and getmetatable(prop_default) ~= "readonly" then
            error(format(
                "default value for '%s' must be read-only, or "..
                "it could be modified unexpectedly",
                prop_name), 3)
        end
    else
        prop_default = type_default
    end

    local prop_guard

    if prop_type == "any" then
        if optional then
            prop_guard = function(prop_name, value)
                -- do nothing
            end
        else
            prop_guard = function(prop_name, value)
                if value == nil then
                    error(format(
                        "invalid value for '%s' (non-nil value expected)",
                        prop_name), 3)
                end
            end
        end
    else
        if optional then
            prop_guard = function(prop_name, value)
                if type(value) ~= prop_type
                    and value ~= nil then
                    error(format(
                        "invalid value for '%s' (%s or nil expected, got %s)",
                        prop_name, prop_type, type(value)), 3)
                end
            end
        else
            prop_guard = function(prop_name, value)
                if type(value) ~= prop_type then
                    error(format(
                        "invalid value for '%s' (%s expected, got %s)",
                        prop_name, prop_type, type(value)))
                end
            end
        end
    end

    prop_guard(prop_name, prop_default) -- check default value
    return property_entry {prop_type, prop_guard, prop_default}
end

local function register_pattern_prop(prop_name, pat, prop_default)
    local function guard(prop_name, value)
        if not pat:match(value) then
            error(format(
                "invalid value for '%s' (pattern: %s)",
                prop_name, pat:get_description()), 3)
        end
    end

    if prop_default then
        guard(prop_name, prop_default)
    elseif pat:has_default() then
        prop_default = pat:get_default()
    end

    return property_entry {pat, guard, prop_default}
end

local function create_struct(self, config)
    local s = {}
    s.__index = s

    local props = {}
    s.props = props

    local function trigger_callbacks(s, prop_name, value)
        local funcs = s[CALLBACKS_KEY][prop_name]
        if funcs then
            for f in pairs(funcs) do
                f(s, prop_name, value)
            end
        end
    end

    for prop_name, prop_cfg in pairs(config) do
        local entry

        if type(prop_cfg) == "string" then
            entry = register_raw_prop(prop_name, prop_cfg)
        elseif getmetatable(prop_cfg) == pattern then
            entry = register_pattern_prop(prop_name, prop_cfg)
        else
            guard.table("property config", prop_cfg)

            local prop_type = prop_cfg[1]
            local prop_default = prop_cfg[2]

            if getmetatable(prop_type) == pattern then
                entry = register_pattern_prop(
                    prop_name, prop_type, prop_default)
            else
                entry = register_raw_prop(
                    prop_name, prop_type, prop_default)
            end
        end

        props[prop_name] = entry

        local prop_type = entry[1]
        local prop_guard = entry[2]

        local setter_prefix, getter_prefix

        if prop_type == "boolean" then
            setter_prefix = "set_"
            getter_prefix = "is_"
        else
            setter_prefix = "set_"
            getter_prefix = "get_"
        end

        local setter_name = setter_prefix..prop_name

        s[setter_name] = function(self, value)
            prop_guard(prop_name, value)
            self[prop_name] = value
            trigger_callbacks(self, prop_name, value)
        end

        s["unsafe_"..setter_name] = function(self, value)
            self[prop_name] = value
            trigger_callbacks(self, prop_name, value)
        end
        
        s[getter_prefix..prop_name] = function(self)
            return self[prop_name]
        end
    end

    function s:set(prop_name, value)
        local entry = props[prop_name]
        local prop_guard = entry[2]

        prop_guard(prop_name, value)
        self[prop_name] = value
        trigger_callbacks(self, prop_name, value)
    end

    function s:unsafe_set(prop_name, value)
        local entry = props[prop_name]
        local prop_guard = entry[2]

        self[prop_name] = value
        trigger_callbacks(self, prop_name, value)
    end

    function s:get(prop_name)
        return self[prop_name]
    end

    function s:reset(prop_name)
        if prop_name then
            local prop_default = props[prop_name][3]
            self[prop_name] = prop_default
            trigger_callbacks(self, prop_name, prop_default)
        else
            for prop_name, prop_cfg in pairs(props) do
                local prop_default = prop_cfg[3]
                self[prop_name] = prop_default
                trigger_callbacks(self, prop_name, prop_default)
            end
        end
    end

    function s:notify(prop_name, callback)
        self:notify_change(prop_name, callback)
        callback(self, prop_name, self[prop_name])
    end

    local function raw_notify_change(self, prop_name, callback)
        local entries = self[CALLBACKS_KEY]
        local funcs = entries[prop_name]

        if not funcs then
            funcs = {}
            entries[prop_name] = funcs
        end

        funcs[callback] = true
    end

    function s:notify_change(prop_name, callback)
        guard.nonempty_string("prop_name", prop_name)
        guard.callable("callback", callback)

        if not props[prop_name] then
            error("invalid property: "..prop_name)
        end

        raw_notify_change(self, prop_name, callback)
    end

    function s:unnotify(prop_name, callback)
        guard.nonempty_string("prop_name", prop_name)
        guard.callable("callback", callback)

        if not props[prop_name] then
            error("invalid property: "..prop_name)
        end

        local entries = self[CALLBACKS_KEY]
        local funcs = entries[prop_name]

        if not funcs or not funcs[callback] then
            return
        end

        funcs[callback] = nil
    end

    function s:create_mapper(target, entries)
        guard.table("entries", entries)

        local prop_map = {}

        for i = 1, #entries do
            local entry = entries[i]

            local prop_name
            local target_field_name
            local mapper

            if type(entry) == "string" then
                prop_name = entry
                target_field_name = entry
            else
                prop_name = entry[1]
                target_field_name = entry[2]
                mapper = entry[3]

                guard.nonempty_string(
                    "prperty name", prop_name)

                if type(target_field_name) ~= "string" then
                    -- simple property handler
                    mapper = entry[2]
                    target_field_name = nil
                    guard.callable("property handler", mapper)
                else
                    guard.nonempty_string(
                        "target field name", target_field_name)
                    if mapper then
                        guard.callable("property mapper", mapper)
                    end
                end
            end

            if not props[prop_name] then
                error("invalid property: "..prop_name)
            end

            if mapper then
                if target_field_name then
                    prop_map[prop_name] = {target_field_name, mapper}
                else
                    prop_map[prop_name] = mapper
                end
            else
                prop_map[prop_name] = target_field_name
            end
        end

        local function callback(self, prop_name, value)
            local entry = prop_map[prop_name]
            local entry_type = type(entry)

            if entry_type == "string" then
                target[entry] = value
            elseif entry_type == "table" then
                target[entry[1]] = entry[2](value)
            else
                entry(target, value)
            end
        end

        for i = 1, #entries do
            local entry = entries[i]
            local prop_name = type(entry) == "string"
                and entry or entry[1]

            raw_notify_change(self, prop_name, callback)
            callback(self, prop_name, self[prop_name])
        end

        local function unregister()
            -- do not use `entries`: it might have been modified
            for prop_name in pairs(prop_map) do
                self:unnotify(prop_name, callback)
            end
        end

        return unregister
    end

    s.get_properties = function()
        return props
    end

    readonly(props)
    return setmetatable(s, struct)
end

function struct:__call(config)
    local t = setmetatable({[CALLBACKS_KEY] = {}}, self)

    local props = self.props
    for prop_name, entry in pairs(props) do
        t[prop_name] = entry[3] 
    end

    if not config then
        return t
    end

    guard.table("config", config)

    for prop_name, value in pairs(config) do
        local entry = props[prop_name]

        if entry then
            local prop_guard = entry[2]
            prop_guard(prop_name, value)
            t[prop_name] = value
            -- no callback for triggeriing
        elseif type(prop_name) == "string" then
            error("invalid property: "..tostring(prop_name))
        end
    end

    return t
end

return setmetatable(struct, {
    __call = create_struct
})