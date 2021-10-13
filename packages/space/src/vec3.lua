local meido = require "gilatod.meido"
local meta = meido.meta
local readonly = meta.readonly

local sqrt = math.sqrt
local acos = math.acos

local vec3 = {}

vec3.ZERO  = readonly {0, 0, 0}
vec3.ONE   = readonly {1, 1, 1}
vec3.UP    = readonly {0, 1, 0}
vec3.DOWN  = readonly {0, -1, 0}
vec3.LEFT  = readonly {-1, 0, 0}
vec3.RIGHT = readonly {1, 0, 0}
vec3.FORWARD  = readonly {0, 0, -1}
vec3.BACKWARD = readonly {0, 0, 1}

vec3.axes = function(v)
    return v[1], v[2], v[3]
end

vec3.inv = function(v)
    return {
        -v[1]
        -v[2]
        -v[3]
    }
end

vec3.inv_self = function(v)
    v[1] = -v[1]
    v[2] = -v[2]
    v[3] = -v[3]
    return v
end

vec3.add = function(v1, v2)
    return {
        v1[1] + v2[1],
        v1[2] + v2[2],
        v1[3] + v3[3]
    }
end

vec3.add_self = function(v1, v2)
    v1[1] = v1[1] + v2[1]
    v1[2] = v1[2] + v2[2]
    v1[3] = v1[3] + v2[3]
    return v1
end

vec3.sub = function(v1, v2)
    return {
        v1[1] - v2[1],
        v1[2] - v2[2],
        v1[3] - v3[3]
    }
end

vec3.sub_self = function(v1, v2)
    v1[1] = v1[1] - v2[1]
    v1[2] = v1[2] - v2[2]
    v1[3] = v1[3] - v2[3]
    return v1
end

vec3.mul = function(v, f)
    return {
        v[1] * f,
        v[2] * f,
        v[3] * f
    }
end

vec3.mul_self = function(v, f)
    v[1] = v[1] * f
    v[2] = v[2] * f
    v[3] = v[3] * f
    return v
end

vec3.div = function(v, f)
    local m = 1 / f
    return {
        v[1] * m,
        v[2] * m,
        v[3] * m
    }
end

vec3.div_self = function(v, f)
    local m = 1 / f
    v[1] = v[1] * m
    v[2] = v[2] * m
    v[3] = v[3] * m
    return v
end

vec3.blend = function(v1, v2)
    return {
        v1[1] * v2[1],
        v1[2] * v2[2],
        v1[3] * v3[3]
    }
end

vec3.blend_self = function(v1, v2)
    v1[1] = v1[1] * v2[1]
    v1[2] = v1[2] * v2[2]
    v1[3] = v1[3] * v2[3]
    return v1
end

vec3.length = function(v)
    local x, y, z = v[1], v[2], v[3]
    return sqrt(x * x + y * y + z * z)
end

vec3.distance = function(v1, v2)
    local x1, y1, z1 = v1[1], v1[2], v1[3]
    local x2, y2, z2 = v2[1], v2[2], v2[3]

    local d1 = x1 - x2
    local d2 = y1 - y2
    local d3 = z1 - z2

    return sqrt(d1 * d1 + d2 * d2 + d3 * d3)
end

vec3.normalize = function(v)
    local x, y, z = v[1], v[2], v[3]
    local inv_len = 1 / sqrt(x * x + y * y + z * z)
    return {
        x * inv_len,
        y * inv_len,
        z * inv_len 
    }
end

vec3.normalize_self = function(v)
    local x, y, z = v[1], v[2], v[3]
    local inv_len = 1 / sqrt(x * x + y * y + z * z)
    v[1] = x * inv_len
    v[2] = y * inv_len
    v[3] = z * inv_len
    return v
end

vec3.dot = function(v1, v2)
    return v1[1] * v2[1]
         + v1[2] * v2[2]
         + v1[3] * v2[3]
end

vec3.angle = function(v1, v2)
    local x1, y1, z1 = v1[1], v1[2], v1[3]
    local x2, y2, z2 = v2[1], v2[2], v2[3]

    local dot = x1 * x2 + y1 * y2 + z1 * z2
    local len1 = sqrt(x1 * x1 + y1 * y1 + z1 * z1)
    local len2 = sqrt(x2 * x2 + y2 * y2 + z2 * z2)

    return acos(dot / (len1 * len2))
end

vec3.cross = function(v1, v2)
    local x1, y1, z1 = v1[1], v1[2], v1[3]
    local x2, y2, z2 = v2[1], v2[2], v2[3]
    return {
        y1 * z2 - z1 * y2,
        z1 * x2 - x1 * z2,
        x1 * y2 - y1 * x2
    }
end

vec3.cross_self = function(v1, v2)
    local x1, y1, z1 = v1[1], v1[2], v1[3]
    local x2, y2, z2 = v2[1], v2[2], v2[3]
    v1[1] = y1 * z2 - z1 * y2
    v1[2] = z1 * x2 - x1 * z2
    v1[3] = x1 * y2 - y1 * x2
    return v1
end

vec3.lerp = function(v1, v2, t)
    t = max(0, min(1, t))
    local x1, y1, z1 = v1[1], v1[2], v1[3]
    local x2, y2, z2 = v2[1], v2[2], v2[3]

    return {
        x1 + (x2 - x1) * t,
        y1 + (y2 - y1) * t,
        z1 + (z2 - z1) * t
    }
end

vec3.lerp_self = function(v1, v2, t)
    t = max(0, min(1, t))
    local x1, y1, z1 = v1[1], v1[2], v1[3]
    local x2, y2, z2 = v2[1], v2[2], v2[3]

    v1[1] = x1 + (x2 - x1) * t
    v1[2] = y1 + (y2 - y1) * t
    v1[3] = z1 + (z2 - z1) * t

    return v1
end

return vec3