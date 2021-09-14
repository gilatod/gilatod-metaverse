local array = require "gilatod.meido.array"

section("equal", function()
    assert(array.equal({}, {}))
    assert(not array.equal({1}, {1, 2}))
    assert(not array.equal({2, 1}, {1, 2}))
    assert(array.equal({1, 2, 3}, {1, 2, 3}))
end)

section("clone", function()
    local t = {1, 2, 3}
    local t_rep = array.clone(t)

    assert(t ~= t_rep)
    assert(array.equal(t, t_rep))
end)

section("map", function()
    local t = {1, 2, 3}

    array.map(t, function(v) return v + 1 end)

    assert(array.equal(t, {2, 3, 4}))
end)

section("fold", function()
    local t = {1, 2, 3}

    local res = array.fold("", t, function(acc, v)
        return acc..tostring(v)
    end)

    assert(res == "123")
end)

section("fold_rev", function()
    local t = {1, 2, 3}

    local res = array.fold_rev(t, "", function(v, acc)
        return tostring(v)..acc
    end)

    assert(res == "123")
end)

section("reverse", function()
    local t = {1, 2, 3}

    array.reverse(t)

    assert(array.equal(t, {3, 2, 1}))
end)

section("shuffle", function()
    local t = {"a", "b", "c"}

    array.shuffle(t)

    local vs = {}
    vs[t[1]] = true
    vs[t[2]] = true
    vs[t[3]] = true

    assert(vs["a"])
    assert(vs["b"])
    assert(vs["c"])
end)

section("clear", function()
    local t = {"a", "b", "c"}
    
    array.clear(t)

    assert(#t == 0)
end)

section("generate", function()
    local t = array.generate(3, function(i)
        return tostring(i)
    end)

    assert(array.equal(t, {"1", "2", "3"}))

    local t = array.generate(t, function(i, s)
        return s..tostring(i)
    end)

    assert(array.equal(t, {"11", "22", "33"}))
end)