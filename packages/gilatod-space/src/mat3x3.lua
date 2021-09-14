local meido = require "gilatod.meido"
local meido = require "gilatod.meido"
local meta = meido.meta
local readonly = meta.readonly

local sin = math.sin
local cos = math.cos

local mat3x3 = {}

mat3x3.ZERO = meta.readonly {
    0, 0, 0,
    0, 0, 0,
    0, 0, 0
}

mat3x3.zero = function()
    return {
        0, 0, 0,
        0, 0, 0,
        0, 0, 0
    }
end

mat3x3.IDENTITY = meta.readonly {
    1, 0, 0,
    0, 1, 0,
    0, 0, 1
}

mat3x3.identity = function()
    return {
        1, 0, 0,
        0, 1, 0,
        0, 0, 1
    }
end

mat3x3.get_entry = function(m, x, y)
    return m[3 * x + y]
end

mat3x3.set_entry = function(m, x, y, z)
    m[3 * x + y] = z
end

mat3x3.mul = function(m1, m2)
    local m1_1_1, m1_1_2, m1_1_3 = m1[1], m1[2], m1[3]
    local m1_2_1, m1_2_2, m1_2_3 = m1[4], m1[5], m1[6]
    local m1_3_1, m1_3_2, m1_3_3 = m1[7], m1[8], m1[9]

    local m2_1_1, m2_1_2, m2_1_3 = m2[1], m2[2], m2[3]
    local m2_2_1, m2_2_2, m2_2_3 = m2[4], m2[5], m2[6]
    local m2_3_1, m2_3_2, m2_3_3 = m2[7], m2[8], m2[9]

    return {
        m1_1_1 * m2_1_1 + m1_1_2 * m2_2_1 + m1_1_3 * m2_3_1,
        m1_1_1 * m2_1_2 + m1_1_2 * m2_2_2 + m1_1_3 * m2_3_2,
        m1_1_1 * m2_1_3 + m1_1_2 * m2_2_3 + m1_1_3 * m2_3_3,

        m1_2_1 * m2_1_1 + m1_2_2 * m2_2_1 + m1_2_3 * m2_3_1,
        m1_2_1 * m2_1_2 + m1_2_2 * m2_2_2 + m1_2_3 * m2_3_2,
        m1_2_1 * m2_1_3 + m1_2_2 * m2_2_3 + m1_2_3 * m2_3_3,

        m1_3_1 * m2_1_1 + m1_3_2 * m2_2_1 + m1_3_3 * m2_3_1,
        m1_3_1 * m2_1_2 + m1_3_2 * m2_2_2 + m1_3_3 * m2_3_2,
        m1_3_1 * m2_1_3 + m1_3_2 * m2_2_3 + m1_3_3 * m2_3_3
    }
end

mat3x3.mul_self = function(m1, m2)
    local m1_1_1, m1_1_2, m1_1_3 = m1[1], m1[2], m1[3]
    local m1_2_1, m1_2_2, m1_2_3 = m1[4], m1[5], m1[6]
    local m1_3_1, m1_3_2, m1_3_3 = m1[7], m1[8], m1[9]

    local m2_1_1, m2_1_2, m2_1_3 = m2[1], m2[2], m2[3]
    local m2_2_1, m2_2_2, m2_2_3 = m2[4], m2[5], m2[6]
    local m2_3_1, m2_3_2, m2_3_3 = m2[7], m2[8], m2[9]

    m1[1] = m1_1_1 * m2_1_1 + m1_1_2 * m2_2_1 + m1_1_3 * m2_3_1
    m1[2] = m1_1_1 * m2_1_2 + m1_1_2 * m2_2_2 + m1_1_3 * m2_3_2
    m1[3] = m1_1_1 * m2_1_3 + m1_1_2 * m2_2_3 + m1_1_3 * m2_3_3

    m1[4] = m1_2_1 * m2_1_1 + m1_2_2 * m2_2_1 + m1_2_3 * m2_3_1
    m1[5] = m1_2_1 * m2_1_2 + m1_2_2 * m2_2_2 + m1_2_3 * m2_3_2
    m1[6] = m1_2_1 * m2_1_3 + m1_2_2 * m2_2_3 + m1_2_3 * m2_3_3

    m1[7] = m1_3_1 * m2_1_1 + m1_3_2 * m2_2_1 + m1_3_3 * m2_3_1
    m1[8] = m1_3_1 * m2_1_2 + m1_3_2 * m2_2_2 + m1_3_3 * m2_3_2
    m1[9] = m1_3_1 * m2_1_3 + m1_3_2 * m2_2_3 + m1_3_3 * m2_3_3

    return m1
end

mat3x3.vec_mul = function(v, m)
    local x, y, z = v[1], v[2], v[3]
    return {
        x * m[1] + y * m[4] + z * m[7],
        x * m[2] + y * m[5] + z * m[8],
        x * m[3] + y * m[6] + z * m[9],
    }
end

mat3x3.vec_mul_self = function(v, m)
    local x, y, z = v[1], v[2], v[3]
    v[1] = x * m[1] + y * m[4] + z * m[7]
    v[2] = x * m[2] + y * m[5] + z * m[8]
    v[3] = x * m[3] + y * m[6] + z * m[9]
    return v
end

mat3x3.transpose = function(m)
    return {
        m[1], m[4], m[7],
        m[2], m[5], m[8],
        m[3], m[6], m[9]
    }
end

mat3x3.transpose_self = function(m)
    m[1], m[2], m[3],
    m[4], m[5], m[6],
    m[7], m[8], m[9] =
    
    m[1], m[4], m[7],
    m[2], m[5], m[8],
    m[3], m[6], m[9]

    return m
end

return mat3x3