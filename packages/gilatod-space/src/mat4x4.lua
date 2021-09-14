local meido = require "gilatod.meido"
local meta = meido.meta
local readonly = meta.readonly

local mat4x4 = {}

mat4x4.ZERO = readonly {
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0
}

mat4x4.zero = function()
    return {
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
    }
end

mat4x4.IDENTITY = readonly {
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
}

mat4x4.identity = function()
    return {
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1
    }
end

mat4x4.get_entry = function(m, x, y)
    return m[4 * x + y]
end

mat4x4.set_entry = function(m, x, y, z)
    m[4 * x + y] = z
end

mat4x4.mul = function(m1, m2)
    local m1_1_1, m1_1_2, m1_1_3, m1_1_4 = m1[1],  m1[2],  m1[3],  m1[4]
    local m1_2_1, m1_2_2, m1_2_3, m1_2_4 = m1[5],  m1[6],  m1[7],  m1[8]
    local m1_3_1, m1_3_2, m1_3_3, m1_3_4 = m1[9],  m1[10], m1[11], m1[12]
    local m1_4_1, m1_4_2, m1_4_3, m1_4_4 = m1[13], m1[14], m1[15], m1[16]

    local m2_1_1, m2_1_2, m2_1_3, m2_1_4 = m2[1],  m2[2],  m2[3],  m2[4]
    local m2_2_1, m2_2_2, m2_2_3, m2_2_4 = m2[5],  m2[6],  m2[7],  m2[8]
    local m2_3_1, m2_3_2, m2_3_3, m2_3_4 = m2[9],  m2[10], m2[11], m2[12]
    local m2_4_1, m2_4_2, m2_4_3, m2_4_4 = m2[13], m2[14], m2[15], m2[16]

    return {
        m1_1_1 * m2_1_1 + m1_1_2 * m2_2_1 + m1_1_3 * m2_3_1 + m1_1_4 * m2_4_1,
        m1_1_1 * m2_1_2 + m1_1_2 * m2_2_2 + m1_1_3 * m2_3_2 + m1_1_4 * m2_4_2,
        m1_1_1 * m2_1_3 + m1_1_2 * m2_2_3 + m1_1_3 * m2_3_3 + m1_1_4 * m2_4_3,
        m1_1_1 * m2_1_4 + m1_1_2 * m2_2_4 + m1_1_3 * m2_3_4 + m1_1_4 * m2_4_4,

        m1_2_1 * m2_1_1 + m1_2_2 * m2_2_1 + m1_2_3 * m2_3_1 + m1_2_4 * m2_4_1,
        m1_2_1 * m2_1_2 + m1_2_2 * m2_2_2 + m1_2_3 * m2_3_2 + m1_2_4 * m2_4_2,
        m1_2_1 * m2_1_3 + m1_2_2 * m2_2_3 + m1_2_3 * m2_3_3 + m1_2_4 * m2_4_3,
        m1_2_1 * m2_1_4 + m1_2_2 * m2_2_4 + m1_2_3 * m2_3_4 + m1_2_4 * m2_4_4,

        m1_3_1 * m2_1_1 + m1_3_2 * m2_2_1 + m1_3_3 * m2_3_1 + m1_3_4 * m2_4_1,
        m1_3_1 * m2_1_2 + m1_3_2 * m2_2_2 + m1_3_3 * m2_3_2 + m1_3_4 * m2_4_2,
        m1_3_1 * m2_1_3 + m1_3_2 * m2_2_3 + m1_3_3 * m2_3_3 + m1_3_4 * m2_4_3,
        m1_3_1 * m2_1_4 + m1_3_2 * m2_2_4 + m1_3_3 * m2_3_4 + m1_3_4 * m2_4_4,

        m1_4_1 * m2_1_1 + m1_4_2 * m2_2_1 + m1_4_3 * m2_3_1 + m1_4_4 * m2_4_1,
        m1_4_1 * m2_1_2 + m1_4_2 * m2_2_2 + m1_4_3 * m2_3_2 + m1_4_4 * m2_4_2,
        m1_4_1 * m2_1_3 + m1_4_2 * m2_2_3 + m1_4_3 * m2_3_3 + m1_4_4 * m2_4_3,
        m1_4_1 * m2_1_4 + m1_4_2 * m2_2_4 + m1_4_3 * m2_3_4 + m1_4_4 * m2_4_4
    }
end

mat4x4.mul_self = function(m1, m2)
    local m1_1_1, m1_1_2, m1_1_3, m1_1_4 = m1[1],  m1[2],  m1[3],  m1[4]
    local m1_2_1, m1_2_2, m1_2_3, m1_2_4 = m1[5],  m1[6],  m1[7],  m1[8]
    local m1_3_1, m1_3_2, m1_3_3, m1_3_4 = m1[9],  m1[10], m1[11], m1[12]
    local m1_4_1, m1_4_2, m1_4_3, m1_4_4 = m1[13], m1[14], m1[15], m1[16]

    local m2_1_1, m2_1_2, m2_1_3, m2_1_4 = m2[1],  m2[2],  m2[3],  m2[4]
    local m2_2_1, m2_2_2, m2_2_3, m2_2_4 = m2[5],  m2[6],  m2[7],  m2[8]
    local m2_3_1, m2_3_2, m2_3_3, m2_3_4 = m2[9],  m2[10], m2[11], m2[12]
    local m2_4_1, m2_4_2, m2_4_3, m2_4_4 = m2[13], m2[14], m2[15], m2[16]

    m1[1]  = m1_1_1 * m2_1_1 + m1_1_2 * m2_2_1 + m1_1_3 * m2_3_1 + m1_1_4 * m2_4_1
    m1[2]  = m1_1_1 * m2_1_2 + m1_1_2 * m2_2_2 + m1_1_3 * m2_3_2 + m1_1_4 * m2_4_2
    m1[3]  = m1_1_1 * m2_1_3 + m1_1_2 * m2_2_3 + m1_1_3 * m2_3_3 + m1_1_4 * m2_4_3
    m1[4]  = m1_1_1 * m2_1_4 + m1_1_2 * m2_2_4 + m1_1_3 * m2_3_4 + m1_1_4 * m2_4_4

    m1[5]  = m1_2_1 * m2_1_1 + m1_2_2 * m2_2_1 + m1_2_3 * m2_3_2 + m1_2_4 * m2_4_1
    m1[6]  = m1_2_1 * m2_1_2 + m1_2_2 * m2_2_2 + m1_2_3 * m2_3_1 + m1_2_4 * m2_4_2
    m1[7]  = m1_2_1 * m2_1_3 + m1_2_2 * m2_2_3 + m1_2_3 * m2_3_4 + m1_2_4 * m2_4_3
    m1[8]  = m1_2_1 * m2_1_4 + m1_2_2 * m2_2_4 + m1_2_3 * m2_3_3 + m1_2_4 * m2_4_4

    m1[9]  = m1_3_1 * m2_1_1 + m1_3_2 * m2_2_1 + m1_3_3 * m2_3_1 + m1_3_4 * m2_4_1
    m1[10] = m1_3_1 * m2_1_2 + m1_3_2 * m2_2_2 + m1_3_3 * m2_3_2 + m1_3_4 * m2_4_2
    m1[11] = m1_3_1 * m2_1_3 + m1_3_2 * m2_2_3 + m1_3_3 * m2_3_3 + m1_3_4 * m2_4_3
    m1[12] = m1_3_1 * m2_1_4 + m1_3_2 * m2_2_4 + m1_3_3 * m2_3_4 + m1_3_4 * m2_4_4

    m1[13] = m1_4_1 * m2_1_1 + m1_4_2 * m2_2_1 + m1_4_3 * m2_3_1 + m1_4_4 * m2_4_1
    m1[14] = m1_4_1 * m2_1_2 + m1_4_2 * m2_2_2 + m1_4_3 * m2_3_2 + m1_4_4 * m2_4_2
    m1[15] = m1_4_1 * m2_1_3 + m1_4_2 * m2_2_3 + m1_4_3 * m2_3_3 + m1_4_4 * m2_4_3
    m1[16] = m1_4_1 * m2_1_4 + m1_4_2 * m2_2_4 + m1_4_3 * m2_3_4 + m1_4_4 * m2_4_4

    return m1
end

mat4x4.vec_mul = function(v, m)
    local x, y, z = v[1], v[2], v[3]
    return {
        x * m[1] + y * m[5] + z * m[9]  + m[13],
        x * m[2] + y * m[6] + z * m[10] + m[14],
        x * m[3] + y * m[7] + z * m[11] + m[15],
    }
end

mat4x4.vec_mul_self = function(v, m)
    local x, y, z = v[1], v[2], v[3]
    v[1] = x * m[1] + y * m[5] + z * m[9]  + m[13]
    v[2] = x * m[2] + y * m[6] + z * m[10] + m[14]
    v[3] = x * m[3] + y * m[7] + z * m[11] + m[15]
    return v
end

mat4x4.vec_mul_normalized = function(v, m)
    local x, y, z = v[1], v[2], v[3]
    local w = x * m[4] + y * m[8] + z * m[12] + m[16]

    local res_x, res_y, res_z =
        x * m[1] + y * m[5] + z * m[9]  + m[13],
        x * m[2] + y * m[6] + z * m[10] + m[14],
        x * m[3] + y * m[7] + z * m[11] + m[15]

    if w ~= 1 and w ~= 0 then
        local w_rec = 1 / w
        res_x = res_x * w_rec
        res_y = res_y * w_rec
        res_z = res_z * w_rec
    end

    return {res_x, res_y, res_z}
end

mat4x4.vec_mul_self_normalized = function(v, m)
    local x, y, z = v[1], v[2], v[3]
    local w = x * m[4] + y * m[8] + z * m[12] + m[16]

    local res_x, res_y, res_z =
        x * m[1] + y * m[5] + z * m[9]  + m[13],
        x * m[2] + y * m[6] + z * m[10] + m[14],
        x * m[3] + y * m[7] + z * m[11] + m[15]

    if w ~= 1 and w ~= 0 then
        local w_rec = 1 / w
        res_x = res_x * w_rec
        res_y = res_y * w_rec
        res_z = res_z * w_rec
    end

    v[1], v[2], v[3] = res_x, res_y, res_z
    return v
end

mat4x4.dir_mul = function(v, m)
    local x, y, z = v[1], v[2], v[3]
    return {
        x * m[1] + y * m[5] + z * m[9],
        x * m[2] + y * m[6] + z * m[10],
        x * m[3] + y * m[7] + z * m[11]
    }
end

mat4x4.dir_mul_self = function(v, m)
    local x, y, z = v[1], v[2], v[3]
    v[1] = x * m[1] + y * m[5] + z * m[9]
    v[2] = x * m[2] + y * m[6] + z * m[10]
    v[3] = x * m[3] + y * m[7] + z * m[11]
    return v
end

mat4x4.transpose = function(m)
    return {
        m[1], m[5], m[9],  m[13],
        m[2], m[6], m[10], m[14],
        m[3], m[7], m[11], m[15],
        m[4], m[8], m[12], m[16]
    }
end

mat4x4.transpose_self = function(m)
    m[1],  m[2],  m[3],  m[4],
    m[5],  m[6],  m[7],  m[8],
    m[9],  m[10], m[11], m[12],
    m[13], m[14], m[15], m[16] =
    
    m[1], m[5], m[9],  m[13],
    m[2], m[6], m[10], m[14],
    m[3], m[7], m[11], m[15],
    m[4], m[8], m[12], m[16]

    return m
end

return mat4x4