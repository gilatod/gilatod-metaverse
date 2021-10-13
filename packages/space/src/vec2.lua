local meido = require "gilatod.meido"
local meta = meido.meta
local readonly = meta.readonly

local sqrt = math.sqrt
local acos = math.acos
local min = math.min
local max = math.max

local vec2 = {}

vec2.ZERO  = readonly {0, 0}
vec2.ONE   = readonly {1, 1}
vec2.UP    = readonly {0, 1}
vec2.DOWN  = readonly {0, -1}
vec2.LEFT  = readonly {-1, 0}
vec2.RIGHT = readonly {1, 0}

vec2.axes = function(v)
    return v[1], v[2]
end

vec2.inv = function(v)
    return {
        -v[1]
        -v[2]
    }
end

vec2.inv_self = function(v)
    v[1] = -v[1]
    v[2] = -v[2]
    return v
end

vec2.add = function(v1, v2)
    return {
        v1[1] + v2[1],
        v1[2] + v2[2]
    }
end

vec2.add_self = function(v1, v2)
    v1[1] = v1[1] + v2[1]
    v1[2] = v1[2] + v2[2]
    return v1
end

vec2.sub = function(v1, v2)
    return {
        v1[1] - v2[1],
        v1[2] - v2[2]
    }
end

vec2.sub_self = function(v1, v2)
    v1[1] = v1[1] - v2[1]
    v1[2] = v1[2] - v2[2]
    return v1
end

vec2.mul = function(v, f)
    return {
        v[1] * f,
        v[2] * f
    }
end

vec2.mul_self = function(v, f)
    v[1] = v[1] * f
    v[2] = v[2] * f
    return v
end

vec2.div = function(v, f)
    local m = 1 / f
    return {
        v[1] * m,
        v[2] * m
    }
end

vec2.div_self = function(v, f)
    local m = 1 / f
    v[1] = v[1] * m
    v[2] = v[2] * m
    return v
end

vec2.blend = function(v1, v2)
    return {
        v1[1] * v2[1],
        v1[2] * v2[2]
    }
end

vec2.blend_self = function(v1, v2)
    v1[1] = v1[1] * v2[1]
    v1[2] = v1[2] * v2[2]
    return v1
end

vec2.length = function(v)
    local x, y = v[1], v[2]
    return sqrt(x * x + y * y)
end

vec2.distance = function(v1, v2)
    local x1, y1 = v1[1], v1[2]
    local x2, y2 = v2[1], v2[2]

    local d1 = x1 - x2
    local d2 = y1 - y2

    return sqrt(d1 * d1 + d2 * d2)
end

vec2.normalize = function(v)
    local x, y = v[1], v[2]
    local inv_len = 1 / sqrt(x * x + y * y)
    return {
        x * inv_len,
        y * inv_len
    }
end

vec2.normalize_self = function(v)
    local x, y = v[1], v[2]
    local inv_len = 1 / sqrt(x * x + y * y)
    v[1] = x * inv_len
    v[2] = y * inv_len
    return v
end

vec2.dot = function(v1, v2)
    return v1[1] * v2[1]
         + v1[2] * v2[2]
end

vec2.angle = function(v1, v2)
    local x1, y1 = v1[1], v1[2]
    local x2, y2 = v2[1], v2[2]

    local dot = x1 * x2 + y1 * y2
    local len1 = sqrt(x1 * x1 + y1 * y1)
    local len2 = sqrt(x2 * x2 + y2 * y2)

    return acos(dot / (len1 * len2))
end

vec2.lerp = function(v1, v2, t)
    t = max(0, min(1, t))
    local x1, y1 = v1[1], v1[2]
    local x2, y2 = v2[1], v2[2]

    return {
        x1 + (x2 - x1) * t,
        y1 + (y2 - y1) * t,
    }
end

vec2.lerp_self = function(v1, v2, t)
    t = max(0, min(1, t))
    local x1, y1 = v1[1], v1[2]
    local x2, y2 = v2[1], v2[2]

    v1[1] = x1 + (x2 - x1) * t
    v1[2] = y1 + (y2 - y1) * t

    return v1
end

return vec2