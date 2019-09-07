if (bit == nil) then
    require("bit");
end
local select = select;
local type = type;
local bit_band = bit.band;
local bit_bor  = bit.bor;
local bit_bnot = bit.bnot;
local bit_lshift = bit.lshift;
local bit_rshift = bit.rshift;
local math_floor = math.floor;
local MPI_VERSION  = "0.1";
local MPI_REVISION = string.match("$Rev$", "%$Rev: (%\d+) %$");
local MPI_DIGIT_BIT   = 16; 
local MPI_DIGIT_MAX   = 2^MPI_DIGIT_BIT-1;
local MPI_DIGIT_RADIX = MPI_DIGIT_MAX+1;
local MPI_DIGIT_TOP_BIT = 2^(MPI_DIGIT_BIT - 1);
local MPI_DIGIT_FORMAT  = "%0" .. (MPI_DIGIT_BIT / 4) .. "x"
MPI = {};
MPI.VERSION       = MPI_VERSION;
MPI.REVISION      = MPI_REVISION;
MPI.DIGIT_BIT     = MPI_DIGIT_BIT;
MPI.DIGIT_MAX     = MPI_DIGIT_MAX;
MPI.DIGIT_RADIX   = MPI_DIGIT_RADIX;
MPI.DIGIT_TOP_BIT = MPI_DIGIT_TOP_BIT;
MPI.DIGIT_FORMAT  = MPI_DIGIT_FORMAT;
local
function MPI_DIGIT_ACCUM(a)
    return bit_band((a), MPI_DIGIT_MAX);
end
local
function MPI_DIGIT_CARRYOUT(a)
    return MPI_DIGIT_ACCUM(bit_rshift(a, MPI_DIGIT_BIT));
end
local
function ASSERT_PARAM_TYPE(farg, ...)
    local assert_string = '@FUNC@: argument "' .. farg .. '", got "\' .. _type .. \'" expected ';
    local check_string = 'do local _type = type(' .. farg .. '); if (';
    local n=select('#', ...);
    if (n == 1) then
        local a = ...;
        check_string = check_string .. '_type ~= \'' .. a .. '\'';
        assert_string = assert_string .. '"' .. a .. '"';
    elseif (n == 2) then
        local a, b = ...;
        check_string = check_string .. '_type ~= \'' .. a .. '\' and _type ~= \'' .. b .. '\'';
        assert_string = assert_string .. '"' .. a .. '" or "' .. b .. '"';
    elseif (n == 3) then
        local a, b, c = ...;
        check_string = check_string .. '_type ~= \'' .. a .. '\' and _type ~= \'' .. b .. '\' and _type ~= \'' .. c .. '\'';
        assert_string = assert_string .. '"' .. a .. '", "' .. b .. '" or "' .. c .. '"';
    else
        local a = select(1, ...);
        check_string = check_string .. '_type ~= \'' .. a .. '\'';
        assert_string = assert_string .. '"' .. a .. '"';
        for ix=2,n-1 do
            local temp = select(ix, ...);
            check_string = check_string .. ' and _type ~= \'' .. temp .. '\'';
            assert_string = assert_string .. ', "' .. temp .. '"';
        end
        local a = select(n, ...);
        check_string = check_string .. ' and _type ~= \'' .. a .. '\'';
        assert_string = assert_string .. ' and "' .. a .. '"';
    end
    check_string = check_string .. ') then return false, \'' .. assert_string .. '\'; end; end;';
    return check_string;
end
local
function ASSERT_PARAM_VAL(farg, expr, errstr)
    local assert_string = '@FUNC@: argument "@PARAM@" ' .. errstr;
    local check_string = 'do if (' .. expr .. ') then return false, \'' .. assert_string .. '\'; end; end;';
    check_string = string.gsub(check_string, '@PARAM@', farg);
    return check_string;
end
local
function ASSERT_RAW(expr, errstr)
    local assert_string = '@FUNC@: ' .. errstr;
    local check_string = 'do if (' .. expr .. ') then return false, \'' .. assert_string .. '\'; end; end;';
    return check_string;
end
local
function ASSERTION_GEN(func, signature, ...)
    local func = tostring(func) or "ASSERT";
    local signature = tostring(signature) or "self,...";
    if (not signature:match("^[a-zA-Z_0-9, ]*[.]*$")) then
        return nil, "Signature contains invalid characters (" .. signature .. ")";
    end
    local n = select('#', ...);
    local body = "";
    for ix=1,n do
        body = body .. select(ix, ...) .. "\n";
    end
    body = body .. "return true, ''";
    body = string.gsub(body, "@FUNC@", func);
    local def, err = loadstring("return function (" .. signature .. ")\n" .. body .. "\nend", func);
    if (def == nil) then
        return nil, err;
    end
    def = def();
    if (type(def) ~= "function") then
        return nil, "Invalid body";
    end
    return function(...) return def(...) end;
end
local do_sanity = 1;
local sanity_checks = {};
local function sanity_val(mpi)
    local ix;
    local tmp = MPI_DIGIT_FORMAT:format(mpi[1] or 0);
    for ix=2,#mpi do tmp = MPI_DIGIT_FORMAT:format(mpi[ix] or 0) .. tmp; end
    tmp = string.gsub(tmp, '^0*', '');
    return "0x" .. tmp;
end
local s_mpi_coroutine_doing = false;
local s_mpi_coroutine_debug = false;
local s_mpi_coroutine_steps = 0;
local s_mpi_coroutine_total_steps = 1;
local
function s_mpi_coroutine_yield()
    if not s_mpi_coroutine_doing then return; end
    s_mpi_coroutine_steps = s_mpi_coroutine_steps - 1;
    if s_mpi_coroutine_steps > 0 then return; end
    local total_steps = s_mpi_coroutine_total_steps;
    local debug = s_mpi_coroutine_debug;
    if s_mpi_coroutine_debug ~= false then
        print('[wrap]: yield    ' .. s_mpi_coroutine_debug);
    end
    s_mpi_coroutine_doing = false;
    s_mpi_coroutine_debug = false;
    new_steps = coroutine.yield(true);
    if new_steps ~= nil and new_steps > 0 then
        total_steps = new_steps;
    end
    s_mpi_coroutine_total_steps = total_steps;
    s_mpi_coroutine_steps = total_steps;
    s_mpi_coroutine_doing = true;
    s_mpi_coroutine_debug = debug;
    if s_mpi_coroutine_debug ~= false then
        print('[wrap]: continue ' .. s_mpi_coroutine_debug);
    end
end
function mpi_coroutine_wrap(func, debug)
    local debug = debug or false;
    local co = coroutine.wrap(function(steps, ...)
        s_mpi_coroutine_debug = debug;
        s_mpi_coroutine_doing = true;
        s_mpi_coroutine_total_steps = steps;
        if s_mpi_coroutine_debug ~= false then
            print('[wrap]: starting ' .. s_mpi_coroutine_debug);
        end
        local result = func(...)
        if s_mpi_coroutine_debug ~= false then
            print('[wrap]: ending   ' .. s_mpi_coroutine_debug);
        end
        s_mpi_coroutine_debug = false;
        return false, result;
    end)
    return co;
end
local
function s_mpi_clamp(mpi)
    local ix;
    for ix=#mpi,2,-1 do
        if (mpi[ix] == 0) then
            mpi[ix] = nil;
        else
            break;
        end
    end
    if mpi.sign ~= '+' and mpi.sign ~= '-' then
        mpi.sign = '+';
    end
    if mpi.sign == '-' and #mpi == 1 and mpi[1] == 0 then
        mpi.sign = '+';
    end
end
local
function s_mpi_grow(mpi, d)
    while #mpi < d do
        mpi[#mpi + 1] = 0;
    end
end
local
function s_mpi_size(mpi, d)
    while #mpi > d do
        mpi[#mpi] = nil;
    end
    while #mpi < d do
        mpi[#mpi + 1] = 0;
    end
end
local
function s_mpi_zero(mpi, d)
    while #mpi > d do
        mpi[#mpi] = nil;
    end
    for ix=1,#mpi do
        mpi[ix] = 0;
    end
    while #mpi < d do
        mpi[#mpi + 1] = 0;
    end
end
local
function s_mpi_copy(a, b)
    while #a > #b do
        a[#a] = nil;
    end
    for ix=1,#b do
        a[ix] = b[ix];
    end
end
local
function s_mpi_lshd(mpi, p)
    local ix;
    if (p == 0) then
        return;
    end
    for ix=#mpi,1,-1 do
        mpi[ix + p] = mpi[ix];
    end
    for ix=1,p do
        mpi[ix] = 0;
    end
end
local
function s_mpi_rshd(mpi, p)
    local ix;
    if (p == 0) then
        return;
    end
    if (p > #mpi) then
        mpi_zero(mpi);
        return;
    end
    for ix=p+1,#mpi do
        mpi[ix - p] = mpi[ix];
    end
    for ix=#mpi+1-p,#mpi do
        mpi[ix] = 0;
    end
    s_mpi_clamp(mpi);
end
local
function s_mpi_mod_2d(mpi, d)
    local ndig = math_floor(d / MPI_DIGIT_BIT) + 1;
    local nbit = d % MPI_DIGIT_BIT;
    local ix;
    if (ndig > #mpi) then
        return;
    end
    local dmask = bit_lshift(1, nbit) - 1;
    mpi[ndig] = bit_band(mpi[ndig], dmask);
    for ix=ndig+1,#mpi do
        mpi[ix] = 0;
    end
    s_mpi_clamp(mpi);
end
sanity_checks[#sanity_checks + 1] = {"s_mpi_mod_2d", function()
    local a = {MPI_DIGIT_MAX, MPI_DIGIT_MAX, MPI_DIGIT_MAX, sign = '+'};
    local result_a_0 = {MPI_DIGIT_MAX, math_floor(MPI_DIGIT_MAX / (2^(MPI_DIGIT_BIT/2))), sign = '+'};
    local result_a_1 = {MPI_DIGIT_MAX, sign = '+'};
    local tmp;
    s_mpi_mod_2d(a, math_floor(MPI_DIGIT_BIT * 1.5));
    tmp = sanity_val(a);
    assert(tmp == sanity_val(result_a_0));
    s_mpi_mod_2d(a, MPI_DIGIT_BIT);
    tmp = sanity_val(a);
    assert(tmp == sanity_val(result_a_1));
end}
local
function s_mpi_mul_2d(mpi, d)
    local ix;
    s_mpi_lshd(mpi, math_floor(d / MPI_DIGIT_BIT));
    d = d % MPI_DIGIT_BIT;
    local mask = 2 ^ d - 1;
    if d > 0 then
        local save = 0;
        local carry;
        for ix=1,#mpi do
            carry = bit_band(bit_rshift(mpi[ix], (MPI_DIGIT_BIT - d)), mask);
            mpi[ix] = bit_band(bit_bor(bit_lshift(mpi[ix], d), save), MPI_DIGIT_MAX);
            save = carry;
        end
        if (save > 0) then
            mpi[#mpi + 1] = save;
        end
    end
    s_mpi_clamp(mpi);
end
sanity_checks[#sanity_checks + 1] = {"s_mpi_mul_2d", function()
    local a = {1, sign = '+'};
    local result_a_0 = {0, 0, 1, sign = '+'};
    local result_a_1 = {0, 0, 0, 0, 2 ^ (MPI_DIGIT_BIT/2), sign = '+'};
    local ix;
    local tmp;
    s_mpi_mul_2d(a, (MPI_DIGIT_BIT * 2));
    tmp = sanity_val(a);
    assert(tmp == sanity_val(result_a_0));
    s_mpi_mul_2d(a, math_floor(MPI_DIGIT_BIT * 2.5));
    tmp = sanity_val(a);
    assert(tmp == sanity_val(result_a_1));
end}
local
function s_mpi_mul_2(mpi)
    local ix;
    local carry=0;
    local last;
    for ix=1,#mpi do
        last = bit_band(bit_rshift(mpi[ix], (MPI_DIGIT_BIT - 1)), 1);
        mpi[ix] = bit_bor(bit_band(bit_lshift(mpi[ix], 1), MPI_DIGIT_MAX), carry)
        carry = last;
    end
    if (carry > 0) then
        mpi[#mpi + 1] = carry;
    end
end
sanity_checks[#sanity_checks + 1] = {"s_mpi_mul_2", function()
    local a = {1, sign = '+'};
    local result_a = {0, 0, 0, 0, 1, sign = '+'};
    local ix;
    local tmp;
    for ix=1,(MPI_DIGIT_BIT * 4) do
        s_mpi_mul_2(a);
    end
    tmp = sanity_val(a);
    assert(tmp == sanity_val(result_a));
end}
local
function s_mpi_div_2d(mpi, d)
    local ix;
    s_mpi_rshd(mpi, math_floor(d / MPI_DIGIT_BIT));
    d = d % MPI_DIGIT_BIT;
    local mask = 2 ^ d - 1;
    if d > 0 then
        local save = 0;
        local carry;
        for ix=#mpi,1,-1 do
            carry = bit_band(mpi[ix], mask);
            mpi[ix] = bit_bor(bit_rshift(mpi[ix], d), bit_lshift(save,(MPI_DIGIT_BIT - d)));
            save = carry;
        end
    end
    s_mpi_clamp(mpi);
end
sanity_checks[#sanity_checks + 1] = {"s_mpi_div_2d", function()
    local a = {0, 0, 0, 0, 1, sign = '+'};
    local result_a_0 = {0, 0, 1, sign = '+'};
    local result_a_1 = {2 ^ (MPI_DIGIT_BIT / 2), sign = '+'};
    local ix;
    local tmp;
    s_mpi_div_2d(a, (MPI_DIGIT_BIT * 2));
    tmp = sanity_val(a);
    assert(tmp == sanity_val(result_a_0), tmp .. " == " .. sanity_val(result_a_0));
    s_mpi_div_2d(a, math_floor(MPI_DIGIT_BIT * 1.5));
    tmp = sanity_val(a);
    assert(tmp == sanity_val(result_a_1), tmp .. " == " .. sanity_val(result_a_1));
end}
local
function s_mpi_div_2(a, b)
    s_mpi_div_2d(a, 1, b);
end
local
function s_mpi_norm(a, b)
    local t = b[#b];
    local d = 0;
    while (t < (MPI_DIGIT_RADIX / 2)) do
        t = bit_lshift(t, 1);
        d = d + 1;
    end
    if (d ~= 0) then
        s_mpi_mul_2d(a, d);
        s_mpi_mul_2d(b, d);
    end
    return d;
end
local MPI_GT = 1;
local MPI_EQ = 0;
local MPI_LT = -1;
local
function s_mpi_cmp(a, b)
    if (#a > #b) then
        return MPI_GT;
    elseif (#a < #b) then
        return MPI_LT;
    else
        local ix = #a;
        while (ix >= 1) do
            if (a[ix] > b[ix]) then
	            return MPI_GT;
	        elseif(a[ix] < b[ix]) then
	            return MPI_LT;
	        end
            ix = ix - 1;
	    end
        return MPI_EQ;
    end
end
local
function s_mpi_cmp_d(mpi, d)
    if (#mpi > 1) then
        return MPI_GT;
    end
    if (mpi[1] < d) then
        return MPI_LT;
    elseif (mpi[1] > d) then
        return MPI_GT;
    else
        return MPI_EQ;
    end
end
local
function s_mpi_add_d(mpi, d)
    local d = MPI_DIGIT_ACCUM(d);
    local carry = 0;
    local ix = 2;
    carry = mpi[1] + d;
    mpi[1] = MPI_DIGIT_ACCUM(carry);
    carry = MPI_DIGIT_CARRYOUT(carry);
    while (carry > 0 and ix <= #mpi) do
        carry = carry + mpi[ix];
        mpi[ix] = MPI_DIGIT_ACCUM(carry);
        carry = MPI_DIGIT_CARRYOUT(carry);
        ix = ix + 1;
    end
    if (carry > 0) then
        mpi[ix] = carry;
    end
end
local
function s_mpi_sub_d(mpi, d) 
    local d = MPI_DIGIT_ACCUM(d);
    local carry = (MPI_DIGIT_RADIX + mpi[1]) - d;
    mpi[1] = MPI_DIGIT_ACCUM(carry);
    carry = MPI_DIGIT_CARRYOUT(carry) == 0 and 1 or 0;
    local ix=2;
    while carry > 0 and ix <= #mpi do
        carry = (MPI_DIGIT_RADIX + mpi[ix]) - carry;
        mpi[ix] = MPI_DIGIT_ACCUM(carry);
        carry = MPI_DIGIT_CARRYOUT(carry) == 0 and 1 or 0;
        ix = ix + 1;
    end
    s_mpi_clamp(mpi);
    if (carry > 0) then
        print(sanity_val(mpi));
        error("s_mpi_sub_d: carry out exception.");
    end
end
sanity_checks[#sanity_checks + 1] = {"s_mpi_{add,sub}_d", function()
    local a = {1, sign = '+'};
    local result_a_0 = {0, 1, sign = '+'};
    local result_a_1 = {1, sign = '+'};
    local ix;
    local tmp;
    s_mpi_add_d(a, MPI_DIGIT_MAX);
    tmp = sanity_val(a);
    assert(tmp == sanity_val(result_a_0), tmp .. " == " .. sanity_val(result_a_0));
    s_mpi_sub_d(a, MPI_DIGIT_MAX);
    tmp = sanity_val(a);
    assert(tmp == sanity_val(result_a_1), tmp .. " == " .. sanity_val(result_a_1));
end}
local
function s_mpi_mul_d(a, d)
    local ix=1;
    local carry=0;
    local d = MPI_DIGIT_ACCUM(d);
    while ix <= #a do
        carry = (a[ix] * d) + carry;
        a[ix] = MPI_DIGIT_ACCUM(carry);
        carry = MPI_DIGIT_CARRYOUT(carry);
        ix = ix + 1;
    end
    if (carry > 0) then
        a[ix] = carry;
        ix = ix + 1;
    end
    s_mpi_clamp(a);
end
local
function s_mpi_div_d(mpi, d)
    if (d == 0) then
        error("s_mpi_div_d: divide by zero.");
    end
    local ix;
    local carry = 0;
    local temp;
    for ix=#mpi,1,-1 do
        carry = (carry * (2 ^ MPI_DIGIT_BIT)) + mpi[ix];
        if (carry >= d) then
            temp  = math_floor(carry / d);
            carry = carry % d;
        else
            temp = 0;
        end
        mpi[ix] = temp;
    end
    s_mpi_clamp(mpi);
    return carry;
end
sanity_checks[#sanity_checks + 1] = {"s_mpi_{mul,div}_d", function()
    local a = {MPI_DIGIT_MAX, MPI_DIGIT_MAX, MPI_DIGIT_MAX, sign = '+'};
    local result_a_0 = {1, MPI_DIGIT_MAX, MPI_DIGIT_MAX, MPI_DIGIT_MAX - 1, sign = '+'};
    local result_a_1 = {MPI_DIGIT_MAX, MPI_DIGIT_MAX, MPI_DIGIT_MAX, sign = '+'};
    local ix;
    local tmp;
    s_mpi_mul_d(a, MPI_DIGIT_MAX);
    tmp = sanity_val(a);
    assert(tmp == sanity_val(result_a_0), tmp .. " == " .. sanity_val(result_a_0));
    s_mpi_div_d(a, MPI_DIGIT_MAX);
    tmp = sanity_val(a);
    assert(tmp == sanity_val(result_a_1), tmp .. " == " .. sanity_val(result_a_1));
end}
local
function s_mpi_add(a, b)
    local carry = 0;
    local ix = 1;
    s_mpi_grow(a, #b);
    while (ix <= #b) do
        carry = carry + a[ix] + b[ix];
        a[ix] = MPI_DIGIT_ACCUM(carry);
        carry = MPI_DIGIT_CARRYOUT(carry);
        ix = ix + 1;
        s_mpi_coroutine_yield();
    end
    while (carry > 0 and ix <= #a) do
        carry = carry + a[ix];
        a[ix] = MPI_DIGIT_ACCUM(carry);
        carry = MPI_DIGIT_CARRYOUT(carry);
        ix = ix + 1;
        s_mpi_coroutine_yield();
    end
    if (carry > 0) then
        a[ix] = carry;
        ix = ix + 1;
    end
end
local
function s_mpi_sub(a, b)
    local carry = 0;
    local ix = 1;
    while (ix <= #b) do
        carry = (MPI_DIGIT_RADIX + a[ix]) - carry - b[ix];
        a[ix] = MPI_DIGIT_ACCUM(carry);
        carry = MPI_DIGIT_CARRYOUT(carry) == 0 and 1 or 0;
        ix = ix + 1;
        s_mpi_coroutine_yield();
    end
    while (ix <= #a) do
        carry = (MPI_DIGIT_RADIX + a[ix]) - carry;
        a[ix] = MPI_DIGIT_ACCUM(carry);
        carry = MPI_DIGIT_CARRYOUT(carry) == 0 and 1 or 0;
        ix = ix + 1;
        s_mpi_coroutine_yield();
    end
    s_mpi_clamp(a);
    if (carry > 0) then
        error("s_mpi_sub_d: carry out exception.");
    end
end
local
function s_mpi_mul(a, b)
    local jx = 0;
    local ix = 0;
    local temp = {0, sign='+'};
    s_mpi_zero(temp, #a + #b + 1);
    local carry = 0;
    for ix=1,#b do
        if b[ix] ~= 0 then
            carry = 0;
            jx=1;
            while jx <= #a do
                carry = b[ix] * a[jx] + carry + temp[ix + jx - 1];
                temp[ix + jx - 1] = MPI_DIGIT_ACCUM(carry);
                carry = MPI_DIGIT_CARRYOUT(carry);
                jx = jx + 1;
            end
            temp[ix + jx - 1] = carry;
            s_mpi_coroutine_yield();
        end
    end
    s_mpi_clamp(temp);
    s_mpi_copy(a, temp);
end
local
function s_mpi_sqr(a)
    s_mpi_mul(a, a);
end
local
function s_mpi_div(a, b)
    local quot = {0};
    local rem = {0};
    local temp = {0};
    local q = {0};
    local diff = {0};
    local trial = {0};
    if (mpi_cmp_z(b) == MPI_EQ) then
        error("s_mpi_div: divide by zero");
    end
    local cmp = s_mpi_cmp(a, b);
    if cmp == MPI_LT then
        mpi_copy(b, a);
        mpi_zero(a);
        return;
    elseif cmp == MPI_EQ then
        mpi_zero(b);
        mpi_set_d(a, 1);
        return;
    end
    local ix = mpi_ispow2(b);
    if (ix >= 0) then
        mpi_copy(b, a); 
        s_mpi_div_2d(a, ix);
        s_mpi_mod_2d(b, ix);
        return;
    end
    local shifts = s_mpi_norm(a, b);
    s_mpi_copy(rem, a);
    mpi_zero(quot);
    local tryagain = true;
    local trydiff = 0;
    s_mpi_copy(trial, b);
    s_mpi_mul_d(trial, 16);
    while s_mpi_cmp(rem, b) > 0 do
        tryagain = true;
        trydiff = 0;
        while tryagain do
            tryagain = false;
            temp_len = math.max(#rem - #b, 1);
            s_mpi_zero(temp, temp_len)
            s_mpi_coroutine_yield()
            if rem[#rem] >= b[#b] then
                temp[temp_len] = math_floor(rem[#rem] / b[#b]);
            else
                temp[temp_len] = math_floor(((rem[#rem] * MPI_DIGIT_RADIX) + rem[#rem - 1]) / b[#b]);
            end
            if trydiff > 0 then
                temp[temp_len] = temp[temp_len] - trydiff;
            end
            mpi_copy(q, temp);
            s_mpi_mul(temp, b);
            if (s_mpi_cmp(temp, rem) > 0) then
                s_mpi_copy(diff, temp);
                s_mpi_sub(diff, rem);
                if s_mpi_cmp(diff, trial) <= 0 then
                    while (s_mpi_cmp(temp, rem) > 0) do
                        s_mpi_sub(temp, b);
                        s_mpi_sub_d(q, 1);
                    end
                else
                    tryagain = true;
                    trydiff = trydiff + 1;
                end
            end
        end
        s_mpi_sub(rem, temp);
        s_mpi_add(quot, q);
    end
    if (shifts ~= 0) then
        s_mpi_div_2d(rem, shifts);
    end
    s_mpi_clamp(quot);
    s_mpi_clamp(rem);
    mpi_copy(a, quot);
    mpi_copy(b, rem);
end
sanity_checks[#sanity_checks + 1] = {"s_mpi_{mul,div}", function()
    local a = {0, 0, 1, 2, 1};
    local b = {1, 1, 1};
    local c = {0};
    local d = {0};
    local result_a = {MPI_DIGIT_MAX,0,1};
    local result_b = {1};
    mpi_copy(c, b);
    mpi_copy(d, a);
    s_mpi_div(a, b);
    assert(sanity_val(a) == sanity_val(result_a), sanity_val(a) .. " == " .. sanity_val(result_a));
    assert(sanity_val(b) == sanity_val(result_b), sanity_val(b) .. " == " .. sanity_val(result_b));
    s_mpi_mul(a, c);
    s_mpi_add(a, b);
    assert(sanity_val(a) == sanity_val(d), sanity_val(a) .. " == " .. sanity_val(d));
end}
local
function s_mpi_2expt(a, k)
    local dig = math_floor(k / MPI_DIGIT_BIT);
    local bit = k % MPI_DIGIT_BIT;
    mpi_zero(a);
    s_mpi_grow(a, dig + 1);
    a[dig + 1] = bit_lshift(1, bit)
end
local
function s_mpi_reduce(x, m, mu)
    local q = {0, sign = '+'};
    local um = #m;
    mpi_copy(q, x);
    s_mpi_rshd(q, um - 1); 
    s_mpi_mul(q, mu);      
    s_mpi_rshd(q, um + 1); 
    s_mpi_mod_2d(x, MPI_DIGIT_BIT * (um + 1));
    s_mpi_mul(q, m);
    s_mpi_mod_2d(q, MPI_DIGIT_BIT * (um + 1))
    mpi_sub(x, q, x);
    if (mpi_cmp_z(x) < 0) then
        mpi_set(q, 1);
        s_mpi_lshd(q, um + 1);
        mpi_add(x, q, x);
    end
    while (mpi_cmp(x, m) >= 0) do
        s_mpi_sub(x, m);
    end
end
local s_mpi_logtab = {
    [0] = {0.000000000,  0},
    {0.000000000,  0}, {1.000000000,  1}, {0.630929754,  0}, {0.500000000,  2}, 
    {0.430676558,  0}, {0.386852807,  0}, {0.356207187,  0}, {0.333333333,  3}, 
    {0.315464877,  0}, {0.301029996,  0}, {0.289064826,  0}, {0.278942946,  0}, 
    {0.270238154,  0}, {0.262649535,  0}, {0.255958025,  0}, {0.250000000,  4}, 
    {0.244650542,  0}, {0.239812467,  0}, {0.235408913,  0}, {0.231378213,  0}, 
    {0.227670249,  0}, {0.224243824,  0}, {0.221064729,  0}, {0.218104292,  0}, 
    {0.215338279,  0}, {0.212746054,  0}, {0.210309918,  0}, {0.208014598,  0}, 
    {0.205846832,  0}, {0.203795047,  0}, {0.201849087,  0}, {0.200000000,  5}, 
    {0.198239863,  0}, {0.196561632,  0}, {0.194959022,  0}, {0.193426404,  0}, 
    {0.191958720,  0}, {0.190551412,  0}, {0.189200360,  0}, {0.187901825,  0}, 
    {0.186652411,  0}, {0.185449023,  0}, {0.184288833,  0}, {0.183169251,  0}, 
    {0.182087900,  0}, {0.181042597,  0}, {0.180031327,  0}, {0.179052232,  0}, 
    {0.178103594,  0}, {0.177183820,  0}, {0.176291434,  0}, {0.175425064,  0}, 
    {0.174583430,  0}, {0.173765343,  0}, {0.172969690,  0}, {0.172195434,  0}, 
    {0.171441601,  0}, {0.170707280,  0}, {0.169991616,  0}, {0.169293808,  0}, 
    {0.168613099,  0}, {0.167948779,  0}, {0.167300179,  0}, {0.166666667,  6}, 
    };
local s_mpi_valmap = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/";
local
function s_mpi_tovalue(ch, r)
    if (r < 36) then
        ch = string.upper(ch);
    end
    local val = (string.find(s_mpi_valmap, ch) or 0) - 1
    if (val < 0 or val >= r) then
        return -1;
    end
    return val;
end
local
function s_mpi_todigit(val, r, low)
    local low = low and 1 or 0;
    if (val < 0 or val >= r) then
        return nil;
    end
    local ch = string.sub(s_mpi_valmap, val + 1, val + 1);
    if (r <= 36 and low ~= 0) then
        ch = string.lower(ch);
    end
    return ch;
end
local
function s_mpi_outlen(bits, r)
    return math_floor(bits * s_mpi_logtab[r][1] + 0.5);
end
local
function s_mpi_divp(a, vec, size)
    local size = size or #vec;
    local rem;
    size = math.min(size, #vec)
    for ix=1,size do
        rem = mpi_mod_d(a, vec[ix]);
        if (rem == 0) then
            return true, ix;
        end
    end
    return false, 0;
end
local
function s_mpi_setcount_d(d)
    local tmp = d - bit_band(bit_rshift(d, 1),  0xdb6db6db)
                  - bit_band(bit_rshift(d, 2), 0x49249249);
    return bit_band(tmp + bit_rshift(tmp, 3),  0xc71c71c7) % 63;
end
local
function s_mpi_bitcount_d(d)
    local bits = 0;
    while d > 0 do
        bits = bits + 1;
        d = bit_rshift(d, 1);
    end
    return bits;
end
local s_mpi_primetab = {}; 
do local t, m, mc, ml = s_mpi_primetab, "0123456789ABCDEF", "/", 16;
local e, i, l, p = 0, 1, 0, "" ..
"101131313515313551531535731313D351915535519131BB31351955515319D313D59135755" ..
"3573791915357313B73735B1/159551595515531B913551B357979755375373D9B191319D31" ..
"3D313/33797355D35575B35191591915/131355755/51979557B35515B9/113515313B15//1" ..
"557/19D31357315B913135BB7B5357373D35135159/3531/7319B197555/1531B9B7FD53131" ..
"9B55/11F1/557531375919D95B1319B1F153197/1/7357F137F137553B1/551535D531535B5" ..
"5D35B753/9/1973515/5B1F73BD9137553135731591973D9B1531FD35753/1795579BD3551/" ..
"B1973D37B5B35/391F/931B53B5737/5131B/B1555351B3B191F1F5/3F73131/57B59135159" ..
"1B919D535755FB13D5379755/5519D35/119D319D37/1351351B3/3/5B135515/515F5B15BF" ..
"135D31/1/7951919195191957/D919759/15BB1/15355/119D5313/71B5F755/1F13515595B" ..
"B/1153/17/731351B3D/D95BD59B13575913D5535191FB7/135B1555/B5D3797B/1313/7B51" ..
"F55D9D3/D5557531B5315/5513/113B153/955379/FF1531319D53795/3315/D3795575B351" ..
"535191F5/33BD/B5/33/17535D55919B79197B9/7137537/1955159B195557595155597/75/" ..
"51/1379/D7/13195153/17B/1F51B5919159D3/71F1919/33137F551BF735/D1915355753B5" ..
"7B3DB9/75B51/57/195D31597535/DD91B91F1/1/7/15F/151/135197955735191B3551B3D/" ..
"135/3375373D53DB31/D3/755BBD5313/15B753B1B/DF15/5D59B5137955/7D5";
while i <= string.len(p) do e = string.sub(p, i, i); i = i + 1;
while e == mc do l = l + ml; e = string.sub(p, i, i); i = i + 1; end;
l = l + string.find(m, e, 0, true); t[#t + 1] = l; end;
assert(t[513] == 3673 and t[1025] == 8167,
("mpi_primetab: %d ~= 3673 or %d ~= 8167"):format(t[513], t[1025]))
end
MPI.PRIMETAB = s_mpi_primetab;
local assert_mpi_new = ASSERTION_GEN("mpi_new", "num, base",
    ASSERT_PARAM_TYPE('num', 'number', 'table', 'string', 'nil'),
    ASSERT_PARAM_TYPE('base', 'number', 'nil'))
function mpi_new(num, base)
    assert(assert_mpi_new(num, base));
    local mpi = {sign = '+', [1] = 0};
    mpi_set(mpi, num, base);
    return mpi;
end
local assert_mpi_zero = ASSERTION_GEN("mpi_new", "mpi",
    ASSERT_PARAM_TYPE('mpi', 'table'))
function mpi_zero(mpi)
    assert(assert_mpi_zero(mpi));
    local ix;
    for ix=#mpi,2,-1 do
        mpi[ix] = nil;
    end
    mpi[1] = 0;
    mpi.sign = '+';
end
local assert_mpi_set_d = ASSERTION_GEN("mpi_set_d", "mpi, d",
    ASSERT_PARAM_TYPE('mpi', 'table'),
    ASSERT_PARAM_TYPE('d', 'number'));
function mpi_set_d(mpi, d)
    assert(assert_mpi_set_d(mpi, d));
    local ix;
    if d < 0 then
        d = d * -1;
        mpi.sign = '-';
    else
        mpi.sign = '+';
    end
    local d = MPI_DIGIT_ACCUM(d);
    for ix=#mpi,2,-1 do
        mpi[ix] = nil;
    end
    mpi[1] = d;
end
local assert_mpi_copy = ASSERTION_GEN("mpi_copy", "a, b",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'));
function mpi_copy(a, b)
    assert(assert_mpi_copy(a, b));
    s_mpi_copy(a, b);
    if b.sign ~= '+' and b.sign ~= '-' then
        a.sign = '+';
    else
        a.sign = b.sign;
    end
end
local assert_mpi_ispow2 = ASSERTION_GEN("mpi_ispow2", "mpi",
    ASSERT_PARAM_TYPE('mpi', 'table'));
function mpi_ispow2(mpi)
    assert(assert_mpi_ispow2(mpi));
    local pow = 0;
    local ix;
    local d = mpi[#mpi]; 
    while (d > 0 and bit_band(d, 1) == 0) do
        pow = pow + 1;
        d = bit_rshift(d, 1);
    end
    if (d == 1) then
        ix = #mpi - 1;
        while (ix >= 1) do
            if (mpi[ix]) then
	            return -1; 
	        end
	        ix = ix - 1;
        end
        return ((#mpi - 1) * MPI_DIGIT_BIT) + pow;
    end
    return -1;
end
local assert_mpi_ispow2d = ASSERTION_GEN("mpi_ispow2d", "d",
    ASSERT_PARAM_TYPE('d', 'number'));
function mpi_ispow2d(d)
    assert(assert_mpi_ispow2d(d));
    local pow = 0;
    while (d > 0 and bit_band(d, 1) == 0) do
        pow = pow + 1;
        d = bit_rshift(d, 1);
    end
    if (d == 1) then
        return pow;
    end
    return -1;
end
sanity_checks[#sanity_checks + 1] = {"mpi_ispow2d", function()
    local tests = {1, 2, 3, 4, 7, 8};
    local results = {0, 1, -1, 2, -1, 3};
    local ix;
    for ix=1,#tests do
        assert(mpi_ispow2d(tests[ix]) == results[ix])
    end
end}
local assert_mpi_cmp_z = ASSERTION_GEN("mpi_cmp_z", "mpi",
    ASSERT_PARAM_TYPE('mpi', 'table'));
function mpi_cmp_z(mpi)
    assert(assert_mpi_cmp_z(mpi));
    if (mpi.sign == '-') then
        return MPI_LT;
    elseif (#mpi == 1 and mpi[1] == 0) then
        return MPI_EQ;
    else
        return MPI_GT;
    end
end
local assert_mpi_cmp_d = ASSERTION_GEN("mpi_cmp_d", "mpi, d",
    ASSERT_PARAM_TYPE('mpi', 'table'),
    ASSERT_PARAM_TYPE('d', 'number'));
function mpi_cmp_d(mpi, d)
    assert(assert_mpi_cmp_d(mpi, d));
    local sign = '+';
    if d < 0 then
        d = d * -1;
        sign = '-';
    end
    if (mpi.sign or '+') == sign then
        local mag = s_mpi_cmp_d(mpi, d);
        if mag == MPI_EQ then
            return MPI_EQ;
        elseif (mpi.sign or '+') == '+' then
            return mag;
        else
            return -mag;
        end
    elseif ((a.sign or '+') == '+') then
        return MPI_GT;
    else
        return MPI_LT;
    end
end
local assert_mpi_cmp = ASSERTION_GEN("mpi_cmp", "a, b",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'));
function mpi_cmp(a, b)
    assert(assert_mpi_cmp(a, b));
    if ((a.sign or '+') == (b.sign or '+')) then
        local mag = s_mpi_cmp(a, b);
        if (mag == MPI_EQ) then
            return MPI_EQ;
        elseif ((a.sign or '+') == '+') then
            return mag;
        else
            return -mag;
        end
    elseif ((a.sign or '+') == '+') then
        return MPI_GT;
    else
        return MPI_LT;
    end
end
local assert_mpi_abs = ASSERTION_GEN("mpi_abs", "a, b",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table', 'nil'));
function mpi_abs(a, b)
    assert(assert_mpi_abs(a, b));
    if b == nil then
        b = a;
    end
    if a ~= b then
        mpi_copy(b, a);
    end
    b.sign = '+';
end
local assert_mpi_neg = ASSERTION_GEN("mpi_neg", "a, b",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table', 'nil'));
function mpi_neg(a, b)
    assert(assert_mpi_neg(a, b));
    if b == nil then
        b = a;
    end
    if a ~= b then
        mpi_copy(b, a);
    end
    b.sign = (b.sign or '+') == '+' and '-' or '+';
end
local assert_mpi_iseven = ASSERTION_GEN("mpi_iseven", "mpi",
    ASSERT_PARAM_TYPE('mpi', 'table'));
function mpi_iseven(mpi)
    assert(assert_mpi_iseven(mpi));
    return ((mpi[1] % 2) == 0)
end
local assert_mpi_isodd = ASSERTION_GEN("mpi_isodd", "mpi",
    ASSERT_PARAM_TYPE('mpi', 'table'));
function mpi_isodd(mpi)
    assert(assert_mpi_iseven(mpi));
    return not(mpi_iseven(mpi))
end
local assert_mpi_add_d = ASSERTION_GEN("mpi_add_d", "a, b, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'number'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_add_d(a, b, c)
    assert(assert_mpi_add_d(a, b, c));
    if c == nil then
        c = a;
    end
    if a ~= c then
        mpi_copy(c, a);
    end
    local b = MPI_DIGIT_ACCUM(b);
    if (a.sign or '+') == '+' then
        s_mpi_add_d(c, b);
    elseif mpi_cmp_d(c, b) >= 0 then
        s_mpi_sub_d(c, b);
    else
        c.sign = '-';
        c[1] = b - c[1];
    end
end
local assert_mpi_sub_d = ASSERTION_GEN("mpi_sub_d", "a, b, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'number'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_sub_d(a, b, c)
    assert(assert_mpi_sub_d(a, b, c));
    if c == nil then
        c = a;
    end
    if a ~= c then
        mpi_copy(c, a);
    end
    local b = MPI_DIGIT_ACCUM(b);
    if (a.sign or '+') == '-' then
        s_mpi_add_d(c, b);
    elseif mpi_cmp_d(c, b) >= 0 then
        s_mpi_sub_d(c, b);
    else
        mpi_neg(c);
        c[1] = b - c[1];
        c.sign = '-';
    end
    if s_mpi_cmp_d(c, 0) == 0 then
        c.sign = '+';
    end
end
local assert_mpi_mul_d = ASSERTION_GEN("mpi_mul_d", "a, b, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'number'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_mul_d(a, b, c)
    assert(assert_mpi_mul_d(a, b, c));
    if c == nil then
        c = a;
    end
    local b = MPI_DIGIT_ACCUM(b);
    if b == 0 then
        mpi_zero(c);
        return;
    end
    if a ~= c then
        mpi_copy(c, a);
    end
    s_mpi_mul_d(c, b);
end
local assert_mpi_div_d = ASSERTION_GEN("mpi_div_d", "a, b, q",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'number'),
    ASSERT_PARAM_TYPE('q', 'table', 'nil'));
function mpi_div_d(a, b, q)
    assert(assert_mpi_div_d(a, b, q));
    local b = MPI_DIGIT_ACCUM(b);
    if q == nil then
        q = {0, sign = '+'};
    end
    mpi_copy(q, a);
    return s_mpi_div_d(q, b);
end
local assert_mpi_mod_d = ASSERTION_GEN("mpi_mod_d", "a, d",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('d', 'number'));
function mpi_mod_d(a, d)
    local rem;
    assert(assert_mpi_mod_d(a, d));
    if (s_mpi_cmp_d(a, d) > 0) then
        local tmp = {0, sign = '+'};
        rem = mpi_div_d(a, d, tmp);
    else
        if a.sign == '-' then
            rem = d - a[1];
        else
            rem = a[1];
        end
    end
    return rem;
end
local assert_mpi_expt_d = ASSERTION_GEN("mpi_expt_d", "a, d, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('d', 'number'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_expt_d(a, d, c)
    assert(assert_mpi_expt_d(a, d, c));
    local s = {0, sign='+'};
    local x = {0, sign='+'};
    local cs = '+';
    if c == nil then
        c = a;
    end
    mpi_copy(x, a);
    mpi_set_d(s, 1);
    if ((d % 2) == 1) then
        cs = a.sign;
    end
    while (d > 0) do
        if ((d % 2) == 1) then
            s_mpi_mul(s, x);
        end
        d = bit_rshift(d, 1);
        s_mpi_sqr(x);
    end
    s.sign = cs;
    mpi_copy(c, s);
end
local assert_mpi_mul_2 = ASSERTION_GEN("mpi_mul_2", "a, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_mul_2(a, c)
    assert(assert_mpi_mul_2(a, c));
    if c == nil then
        c = a;
    end
    if c ~= a then
        mpi_copy(c, a);
    end
    s_mpi_mul_2(c);
end
local assert_mpi_div_2 = ASSERTION_GEN("mpi_div_2", "a, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_div_2(a, c)
    assert(assert_mpi_div_2(a, c));
    if c == nil then
        c = a;
    end
    if c ~= a then
        mpi_copy(c, a);
    end
    s_mpi_div_2(c);
end
local assert_mpi_mul_2d = ASSERTION_GEN("mpi_mul_2d", "a, d, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('d', 'number'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_mul_2d(a, d, c)
    assert(assert_mpi_mul_2d(a, d, c))
    if c == nil then
        c = a;
    end
    local d = MPI_DIGIT_ACCUM(d);
    if a ~= c then
        mpi_copy(c, a);
    end
    if b == 0 then
        return;
    end
    s_mpi_mul_2d(c, d);
end
local assert_mpi_div_2d = ASSERTION_GEN("mpi_div_2d", "a, d, q, r",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('d', 'number'),
    ASSERT_PARAM_TYPE('q', 'table'),
    ASSERT_PARAM_TYPE('r', 'table', 'nil'));
function mpi_div_2d(a, d, q, r)
    assert(assert_mpi_div_2d(a, d, q, r));
    local d = MPI_DIGIT_ACCUM(d);
    if q ~= nil then
        mpi_copy(q, a);
        s_mpi_div_2d(q, d);
    end
    if r ~= nil then
        mpi_copy(r, a);
        s_mpi_mod_2d(r, d);
    end
end
local assert_mpi_add = ASSERTION_GEN("mpi_add", "a, b, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_add(a, b, c)
    assert(assert_mpi_add(a, b, c));
    if c == nil then
        c = a;
    end
    local a_sign = a.sign or '+';
    local b_sign = b.sign or '+';
    if a_sign == b_sign then
        if c == b then
            s_mpi_add(c, a);
        else
            if c ~= a then
                mpi_copy(c, a);
            end
            s_mpi_add(c, b);
        end
        c.sign = a_sign;
    else
        local cmp = s_mpi_cmp(a, b);
        if cmp > 0 then
            if c == b then
                local temp = {0};
                mpi_copy(temp, a);
                s_mpi_sub(temp, b);
                mpi_copy(c, temp);
            else
                if c ~= a then
                    mpi_copy(c, a);
                end
                s_mpi_sub(c, b);
            end
        elseif cmp == 0 then
            mpi_zero(c);
            return;
        else
            if c == a then
                local temp = {0};
                mpi_copy(temp, b);
                s_mpi_sub(temp, a);
                mpi_copy(c, temp);
            else
                if c ~= b then
                    mpi_copy(c, b);
                end
                s_mpi_sub(c, a);
            end
        end
    end
    if #c == 1 and c[1] == 0 then
        c.sign = '+';
    end
end
local assert_mpi_sub = ASSERTION_GEN("mpi_sub", "a, b, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_sub(a, b, c)
    assert(assert_mpi_sub(a, b, c));
    if c == nil then
        c = a;
    end
    local a_sign = a.sign or '+';
    local b_sign = b.sign or '+';
    if a_sign ~= b_sign then
        if c == a then
            s_mpi_add(c, b);
        else
            if c ~= b then
                mpi_copy(c, b);
            end
            s_mpi_add(c, a);
            c.sign = a_sign;
        end
    else
        local cmp = s_mpi_cmp(a, b);
        if cmp > 0 then
            if c == b then
                local temp = {0};
                mpi_copy(temp, a);
                s_mpi_sub(temp, b);
                mpi_copy(c, temp);
            else
                if c ~= a then
                    mpi_copy(c, a);
                end
                s_mpi_sub(c, b);
            end
        elseif cmp == 0 then
            mpi_zero(c);
            return;
        else
            if c == a then
                local temp = {0};
                mpi_copy(temp, b);
                s_mpi_sub(temp, a);
                mpi_copy(c, temp);
            else
                if c ~= b then
                    mpi_copy(c, b);
                end
                s_mpi_sub(c, a);
            end
            c.sign = (a.sign or '+') == '+' and '-' or '+';
        end
    end
    if #c == 1 and c[1] == 0 then
        c.sign = '+';
    end
end
sanity_checks[#sanity_checks + 1] = {"mpi_{add,sub}", function()
    local tests = {
        mpi_new("+1"),
        mpi_new("-1"),
        mpi_new("+0xFF", 16),
        mpi_new("-0xFF", 16),
        mpi_new("+0xFFFF00FF", 16),
        mpi_new("-0xFFFF00FF", 16),
        mpi_new("+0xFFFFFFFF", 16),
        mpi_new("-0xFFFFFFFF", 16),
    };
    local results = {
        [1] = {[3] = mpi_new("+100", 16)},
        [2] = {[6] = mpi_new("-ffff0100", 16)},
        [3] = {[5] = mpi_new("+ffff01fe", 16)},
        [5] = {[7] = mpi_new("+1ffff00fe", 16)},
        [8] = {[4] = mpi_new("-1000000fe", 16)},
    };
    local a   = {0, sign = '+'};
    local b   = {0, sign = '+'};
    local b_n = {0, sign = '+'};
    local c   = {0, sign = '+'};
    local d   = {0, sign = '+'};
    local e   = {0, sign = '+'};
    for ix=1,#tests do
        for jx=1,#tests do
            mpi_copy(a, tests[ix]);
            mpi_copy(b, tests[jx]);
            mpi_neg(b, b_n);
            mpi_add(a, b, c);
            mpi_sub(a, b_n, d);
            if results[ix] ~= nil and results[ix][jx] ~= nil then
                assert(s_mpi_cmp(c, results[ix][jx]) == 0);
            end
            assert(s_mpi_cmp(c, d) == 0);
        end
    end
end}
local assert_mpi_mul = ASSERTION_GEN("mpi_mul", "a, b, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_mul(a, b, c)
    assert(assert_mpi_mul(a, b, c))
    if c == nil then
        c = a;
    end
    local sign = ((a.sign or '+') == (b.sign or '+')) and '+' or '-';
    if (c == b) then
        s_mpi_mul(c, a);
    else
        mpi_copy(c, a);
        s_mpi_mul(c, b);
    end
    if sign == '+' or s_mpi_cmp_d(c, 0) == MPI_EQ then
        c.sign = '+';
    else
        c.sign = sign;
    end
end
local assert_mpi_sqr = ASSERTION_GEN("mpi_sqr", "a, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_sqr(a, c)
    assert(assert_mpi_sqr(a, c))
    mpi_mul(a, a, c);
end
local assert_mpi_div = ASSERTION_GEN("mpi_div", "a, b, q, r",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'),
    ASSERT_PARAM_TYPE('q', 'table', 'nil'),
    ASSERT_PARAM_TYPE('r', 'table', 'nil'),
    ASSERT_PARAM_VAL('b', "mpi_cmp_z(@PARAM@) == MPI_EQ", 'divide by zero.'));
function mpi_div(a, b, q, r)
    assert(assert_mpi_div(a, b, q, r));
    local qtmp = {0, sign = '+'};
    local rtmp = {0, sign = '+'};
    local cmp;
    cmp = s_mpi_cmp(a, b);
    if cmp < 0 then
        if r ~= nil then
            mpi_copy(r, a);
        end
        if q ~= nil then
            mpi_zero(q);
        end
        return;
    elseif (cmp == 0) then
        if q ~= nil then
            mpi_set_d(q, 1);
            if ((a.sign or '+') ~= (b.sign or '+')) then
                q.sign = '-';
            end
        end
        if r ~= nil then
            mpi_zero(r);
        end
        return;
    end
    mpi_copy(qtmp, a);
    mpi_copy(rtmp, b);
    s_mpi_div(qtmp, rtmp);
    rtmp.sign = (a.sign or '+') 
    if (a.sign or '+') == (b.sign or '+') then
        qtmp.sign = '+';    
    else
        qtmp.sign = '-';    
    end
    if s_mpi_cmp_d(qtmp, 0) == MPI_EQ then
        qtmp.sign = '+';
    end
    if s_mpi_cmp_d(rtmp, 0) == MPI_EQ then
        rtmp.sign = '+';
    end
    if q ~= nil then 
        mpi_copy(q, qtmp);
    end
    if r ~= nil then
        mpi_copy(r, rtmp);
    end
end
local assert_mpi_mod = ASSERTION_GEN("mpi_mod", "a, m, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('m', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'),
    ASSERT_PARAM_VAL('m', "(@PARAM@.sign or '+') == '-'", 'must be positive.'));
function mpi_mod(a, m, c)
    assert(assert_mpi_mod(a, m, c));
    local mag;
    if c == nil then
        c = a;
    end
    mag = s_mpi_cmp(a, m);
    if mag > 0 then
        mpi_div(a, m, NULL, c);
        if c.sign == '-' then
            mpi_add(c, m, c);
        end
    elseif mag < 0 then
        mpi_copy(c, a);
        if (mpi_cmp_z(a) < 0) then
            mpi_add(c, m, c);
        end
    else
        mpi_zero(c);
    end
end
sanity_checks[#sanity_checks + 1] = {"mpi_{mul,div}", function()
    local tests = {
        mpi_new("+0xFEF"),
        mpi_new("+0xFFEF"),
        mpi_new("+0xFFEFF"),
        mpi_new("+0xFEFFEF"),
    };
    local a   = {0, sign = '+'};
    local b   = {0, sign = '+'};
    local c   = {0, sign = '+'};
    local d   = {0, sign = '+'};
    local q   = {0, sign = '+'};
    local r   = {0, sign = '+'};
    local r2  = {0, sign = '+'};
    local ext_r = {0, sign = '+'};
    for ix=1,#tests do
        for jx=1,#tests do
            for lx=1,#tests do
                mpi_copy(a, tests[ix]);
                mpi_copy(b, tests[jx]);
                mpi_copy(c, tests[lx]);
                mpi_mul(a, b, d);
                local output = "";
                if mpi_cmp(a, c) > 0 and mpi_cmp(b, c) > 0 then
                    mpi_copy(ext_r, c);
                    mpi_add(d, c);
                    output = " + " .. mpi_tostring(c, 16);
                else
                    mpi_zero(ext_r);
                end
                mpi_div(d, b, q, r);
                mpi_mod(d, b, r2);
                assert(mpi_cmp(a, q) == 0);
                assert(mpi_cmp(ext_r, r) == 0);
                assert(mpi_cmp(ext_r, r2) == 0);
            end
        end
    end
end}
local assert_mpi_expt = ASSERTION_GEN("mpi_expt", "a, b, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'),
    ASSERT_PARAM_VAL('b', "(@PARAM@.sign or '+') == '-'", 'must be positive.'));
function mpi_expt(a, b, c)
    assert(assert_mpi_expt(a, b, c));
    local d;
    local s = {1, sign = '+'};
    local x = {0, sign = '+'};
    mpi_copy(x, a);
    for dig=1,#b-1 do
        d = b[dig];
        bit = 0;
        while bit < MPI_DIGIT_BIT do
            if (bit_band(d, 1) == 1) then
	            s_mpi_mul(s, x);
	        end
            d = bit_rshift(d, 1);
            s_mpi_mul(x, x);
        end
    end
    d = b[#b];
    while (d > 0) do
        if (bit_band(d, 1) == 1) then
            s_mpi_mul(s, x);
        end
        d = bit_rshift(d, 1);
        s_mpi_mul(x, x);
    end
    if (mpi_iseven(b)) then
        s.sign = a.sign or '+';
    end
    mpi_copy(c, s);
    return res;
end
local assert_mpi_2expt = ASSERTION_GEN("mpi_2expt", "a, k",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('k', 'number'));
function mpi_2expt(a, k)
    assert(assert_mpi_2expt(a, k));
    s_mpi_2expt(a, k);
end
local assert_mpi_divis = ASSERTION_GEN("mpi_divis", "a, b",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'));
function mpi_divis(a, b)
    assert(assert_mpi_divis(a, b));
    local rem = {0, sign='+'};
    if (mpi_cmp_z(b) == 0) then
        return false;
    end
    mpi_mod(a, b, rem);
    if (mpi_cmp_z(rem) == 0) then
        return true;
    else
        return false;
    end
end
local assert_mpi_divis_d = ASSERTION_GEN("mpi_divis_d", "a, d",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('d', 'number'));
function mpi_divis_d(a, d)
    assert(assert_mpi_divis_d(a, d));
    if (d == 0) then
        return false;
    end
    local rem = mpi_mod_d(a, d);
    if (rem == 0) then
        return true;
    else
        return false;
    end
end
local assert_mpi_divis_vector = ASSERTION_GEN("mpi_divis_vector", "a, vec, size",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('vec', 'table'),
    ASSERT_PARAM_TYPE('size', 'number', 'nil'));
function mpi_divis_vector(a, vec, size)
    assert(assert_mpi_divis_vector(a, vec, size));
    local size = size or #vec;
    size = size < 1 and 1 or (size > #vec and #vec or size);
    return s_mpi_divp(a, vec, size);
end
local assert_mpi_divis_primes = ASSERTION_GEN("mpi_divis_primes", "a, np",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('np', 'number', 'nil'));
function mpi_divis_primes(a, np)
    assert(assert_mpi_divis_primes(a, np));
    local np = np or #s_mpi_primetab;
    np = np < 1 and 1 or (np > #s_mpi_primetab and #s_mpi_primetab or np);
    local res;
    local which;
    res, which = mpi_divis_vector(a, s_mpi_primetab, np);
    if (res) then
        return res, s_mpi_primetab[which];
    end
    return res, nil;
end
local assert_mpi_fermat = ASSERTION_GEN("mpi_fermat", "a, w",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('w', 'number'));
function mpi_fermat(a, w)
    local base = {0, sign='+'};
    local test = {0, sign='+'};
    mpi_set_d(base, w);
    mpi_exptmod(base, a, a, test);
    if (mpi_cmp(base, test) == MPI_EQ) then
        return true;
    else
        return false;
    end
end
local assert_mpi_random = ASSERTION_GEN("mpi_random", "a",
    ASSERT_PARAM_TYPE('a', 'table'));
function mpi_random(a)
    assert(assert_mpi_random(a));
    for ix=1,#a do
        a[ix] = math_floor(math.random(0, MPI_DIGIT_MAX));
    end
end
local assert_mpi_random_size = ASSERTION_GEN("mpi_random_size", "a, size",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('size', 'number'),
    ASSERT_PARAM_VAL('size', "@PARAM@ < 1", 'must be greater than zero.'));
function mpi_random_size(a, size)
    assert(assert_mpi_random_size(a, size));
    s_mpi_zero(a, size);
    mpi_random(a);
end
local assert_mpi_random_bits = ASSERTION_GEN("mpi_random_bits", "a, bits",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('bits', 'number'),
    ASSERT_PARAM_VAL('bits', "@PARAM@ < 1", 'must be greater than zero.'));
function mpi_random_bits(a, bits)
    assert(assert_mpi_random_bits(a, bits));
    local digits = math_floor(bits / MPI_DIGIT_BIT) + 1;
    s_mpi_zero(a, digits);
    mpi_random(a);
    while mpi_bitcount(a) > bits do
        mpi_div_2(a);
    end
end
local assert_mpi_pprime = ASSERTION_GEN("mpi_pprime", "a, nt",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('nt', 'number'));
function mpi_pprime(a, nt)
    assert(assert_mpi_pprime(a, nt));
    local x = {0, sign='+'};
    local m = {0, sign='+'};
    local z = {0, sign='+'};
    local amo = {0, sign='+'};
    mpi_copy(amo, a);
    s_mpi_size(x, #a);
    mpi_sub_d(amo, 1, amo);
    mpi_copy(m, amo);
    local b = 0;
    while ((m[1] % 2) == 0) do
        mpi_div_2(m, m);
        b = b + 1;
    end
    local res = true;
    for iter=1,nt do
        mpi_random_size(x, #a);
        mpi_mod(x, a, x);
        mpi_exptmod(x, m, a, z);
        if (mpi_cmp_d(z, 1) == MPI_EQ or mpi_cmp(z, amo) == MPI_EQ) then
             return true;
        end
        local jx = 0;
        while (1) do
            if (jx > 0 and mpi_cmp_d(z, 1) == MPI_EQ) then
                return false;
            end
            jx = jx + 1;
            if (jx < b and mpi_cmp(z, amo) ~= MPI_EQ) then
	            mpi_sqrmod(z, a, z);
	        elseif (mpi_cmp(z, amo) == MPI_EQ) then
	            break;
            elseif (jx == b and mpi_cmp(z, amo) ~= MPI_EQ) then
                return false;
	        end
	    end
    end
    return true;
end
local assert_mpi_setcount_d = ASSERTION_GEN("mpi_setcount_d", "d",
    ASSERT_PARAM_TYPE('d', 'number'));
function mpi_setcount_d(d)
    assert(assert_mpi_setcount_d(d));
    return s_mpi_setcount_d(d);
end
local assert_mpi_clrcount_d = ASSERTION_GEN("mpi_clrcount_d", "d",
    ASSERT_PARAM_TYPE('d', 'number'));
function mpi_clrcount_d(d)
    assert(assert_mpi_clrcount_d(d));
    return s_mpi_bitcount_d(MPI_DIGIT_ACCUM(d)) - mpi_setcount_d(MPI_DIGIT_ACCUM(d));
end
local assert_mpi_setcount = ASSERTION_GEN("mpi_setcount", "a",
    ASSERT_PARAM_TYPE('a', 'table'));
function mpi_setcount(a)
    assert(assert_mpi_setcount(a));
    local bits = 0;
    for ix=1,#a do
        bits = bits + s_mpi_setcount_d(a[ix]);
    end
    return bits;
end
local assert_mpi_clrcount = ASSERTION_GEN("mpi_clrcount", "a",
    ASSERT_PARAM_TYPE('a', 'table'));
function mpi_clrcount(a)
    assert(assert_mpi_clrcount(a));
    local bits = 0;
    for ix=1,#a-1 do
        bits = bits + MPI_DIGIT_BIT - s_mpi_setcount_d(a[ix]);
    end
    bits = bits + s_mpi_bitcount_d(a[#a]) - s_mpi_setcount_d(a[#a]);
    return math_floor(bits);
end
local assert_mpi_bitcount_d = ASSERTION_GEN("mpi_bitcount_d", "d",
    ASSERT_PARAM_TYPE('d', 'number'));
function mpi_bitcount_d(d)
    assert(assert_mpi_bitcount_d(d))
    return s_mpi_bitcount_d(MPI_DIGIT_ACCUM(d));
end
local assert_mpi_bitcount = ASSERTION_GEN("mpi_bitcount", "a",
    ASSERT_PARAM_TYPE('a', 'table'));
function mpi_bitcount(a)
    assert(assert_mpi_bitcount(a));
    return ((#a - 1) * MPI_DIGIT_BIT) + s_mpi_bitcount_d(a[#a]);
end
local assert_mpi_not = ASSERTION_GEN("mpi_not", "a, b",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table', 'nil'));
function mpi_not(a, b)
    assert(assert_mpi_not(a, b));
    local ix;
    if b == nil then
        b = a;
    end
    mp_copy(b, a);
    for ix=1,#b do
        b[ix] = bit_band(bit_bnot(b[ix]), MPI_DIGIT_MAX);
    end
    s_mpi_clamp(b);
end
local assert_mpl_and = ASSERTION_GEN("mpl_and", "a, b, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_and(a, b, c)
    assert(assert_mpi_and(a, b, c))
    local ix;
    if c == nil then
        c = a;
    end
    local which = a;
    local other = b;
    if #a > #b then
        which = b;
        other = a;
    end
    mpi_copy(c, which);
    for ix=1,#which do
        c[ix] = bit_band(c[ix], other[ix]);
    end
    s_mpi_clamp(c);
end
local assert_mpl_or = ASSERTION_GEN("mpl_or", "a, b, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_or(a, b, c)
    assert(assert_mpi_or(a, b, c));
    local ix;
    if c == nil then
        c = a;
    end
    local which = a;
    local other = b;
    if #a < #b then
        which = b;
        other = a;
    end
    mpi_copy(c, which);
    for ix=1,#which do
        c[ix] = bit_bor(c[ix], other[ix]);
    end
    s_mpi_clamp(c);
end
local assert_mpl_xor = ASSERTION_GEN("mpl_xor", "a, b, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpl_xor(a, b, c)
    assert(assert_mpl_xor(a, b, c));
    local ix;
    if c == nil then
        c = a;
    end
    local which = a;
    local other = b;
    if #a < #b then
        which = b;
        other = a;
    end
    mpi_copy(c, which);
    for ix=1,#which do
        c[ix] = bit_band(bit_bxor(c[ix], other[ix]), MPI_DIGIT_MAX);
    end
    s_mpi_clamp(c);
end
local assert_mpi_sqrt = ASSERTION_GEN("mpi_sqrt", "a, b",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table', 'nil'),
    ASSERT_PARAM_VAL('a', '@PARAM@.sign == "-"', 'must be non negative.'));
function mpi_sqrt(a, b)
    assert(assert_mpi_sqrt(a, b));
    local x = {0, sign='+'};
    local t = {0, sign='+'};
    if b == nil then
        b = a;
    end
    if (mpi_cmp_d(a, 0) == MPI_EQ or mpi_cmp_d(a, 1) == MPI_EQ) then
        mpi_copy(b, a);
        return;
    end
    mpi_copy(x, a);
    while true do
        mpi_copy(t, x);
        mpi_sqr(t, t);
        mpi_sub(t, a);
        s_mpi_mul_2(x);
        mpi_div(t, x, t, nil);
        s_mpi_div_2(x);
        if (mpi_cmp_z(t) == MPI_EQ) then
            break;
        end
        mpi_sub(x, t, x);
    end
    mpi_sub_d(x, 1, x);
    mpi_copy(b, x);
end
local assert_mpi_gcd = ASSERTION_GEN("mpi_gcd", "a, b, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'),
    ASSERT_PARAM_TYPE('c', 'table'),
    ASSERT_RAW('mpi_cmp_z(a) == MPI_EQ and mpi_cmp_z(b) == MPI_EQ', 'arguments "a" & "b" must not be zero.'));
function mpi_gcd(a, b, c)
    assert(assert_mpi_gcd(a, b, c));
    if c == nil then
        c = a;
    end
    local u = {0, sign='+'};
    local v = {0, sign='+'};
    local t = {0, sign='+'};
    local k = 0;
    if (mpi_cmp_z(a) == MPI_EQ) then
        mpi_copy(c, b);
        c.sign = '+';
        return;
    elseif (mpi_cmp_z(b) == MPI_EQ) then
        mpi_copy(c, a);
        c.sign = '+';
        return;
    end
    mpi_copy(u, a);
    mpi_copy(v, b);
    u.sign = '+';
    v.sign = '+';
    while (mpi_iseven(u) and mpi_iseven(v)) do
        s_mpi_div_2(u);
        s_mpi_div_2(v);
        k = k + 1;
    end
    if (mpi_isodd(u)) then
        mpi_copy(t, v);
        if (v.sign == '+') then
            t.sign = '-';
        else
            t.sign = '+';
        end
    else
        mpi_copy(t, u);
    end
    while (1) do
        while mpi_iseven(t) do
            s_mpi_div_2(t);
        end
        if (mpi_cmp_z(t) == MPI_GT) then
            mpi_copy(u, t);
        else
            mpi_copy(v, t);
            if (t.sign == '+') then
                v.sign = '-';
            else
                v.sign = '+';
            end
        end
        mpi_sub(u, v, t);
        if (s_mpi_cmp_d(t, 0) == MPI_EQ) then
            break;
        end
    end
    s_mpi_2expt(v, k); 
    mpi_mul(u, v, c); 
end
local assert_mpi_lcm = ASSERTION_GEN("mpi_lcm", "a, b, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'),
    ASSERT_PARAM_TYPE('c', 'table'),
    ASSERT_PARAM_VAL('b', 'mpi_cmp_z(@PARAM@) == 0', 'divide by zero.'));
function mpi_lcm(a, b, c)
    assert(assert_mpi_lcm(a, b, c))
    if c == nil then
        c = a;
    end
    local gcd = {0, sign='+'};
    local prod = {0, sign='+'};
    mpi_mul(a, b, prod);
    mpi_gcd(a, b, gcd);
    mpi_div(prod, gcd, c, nil);
end
local assert_mpi_xgcd = ASSERTION_GEN("mpi_xgcd", "a, b, g, x, y",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'),
    ASSERT_PARAM_TYPE('g', 'table'),
    ASSERT_PARAM_TYPE('x', 'table', 'nil'),
    ASSERT_PARAM_TYPE('y', 'table', 'nil'),
    ASSERT_PARAM_VAL('b', 'mpi_cmp_z(@PARAM@) == 0', 'must not be zero.'));
function mpi_xgcd(a, b, g, x, y)
    assert(assert_mpi_xgcd(a, b, g, x, y));
    local gx = {0, sign='+'};
    local xc = {0, sign='+'};
    local yc = {0, sign='+'};
    local u = {0, sign='+'};
    local v = {0, sign='+'};
    local A = {0, sign='+'};
    local B = {0, sign='+'};
    local C = {0, sign='+'};
    local D = {0, sign='+'};
    mpi_copy(xc, a);
    mpi_abs(xc);
    mpi_copy(yc, b);
    mpi_abs(yc);
    mpi_set_d(gx, 1);
    while (mpi_iseven(xc) and mp_iseven(yc)) do
        s_mp_div_2(xc);
        s_mp_div_2(yc);
        s_mp_mul_2(gx);
    end
    mpi_copy(u, xc);
    mpi_copy(v, yc);
    mpi_set_d(A, 1);
    mpi_set_d(D, 1);
    while (1) do
        while (mpi_iseven(u)) do
            s_mpi_div_2(u);
            if (mpi_iseven(A) and mpi_iseven(B)) then
                s_mpi_div_2(A);
                s_mpi_div_2(B);
            else
                mpi_add(A, yc, A);
                s_mpi_div_2(A);
                mpi_sub(B, xc, B);
                s_mpi_div_2(B);
            end
        end
        while (mpi_iseven(v)) do
            s_mpi_div_2(v);
            if (mpi_iseven(C) and mpi_iseven(D)) then
                s_mpi_div_2(C);
                s_mpi_div_2(D);
            else
                mpi_add(C, yc, C);
                s_mpi_div_2(C);
                mpi_sub(D, xc, D);
                s_mpi_div_2(D);
            end
        end
        if (mpi_cmp(u, v) >= 0) then
            mpi_sub(u, v, u);
            mpi_sub(A, C, A);
            mpi_sub(B, D, B);
        else
            mpi_sub(v, u, v);
            mpi_sub(C, A, C);
            mpi_sub(D, B, D);
        end
        if (mpi_cmp_z(u) == 0) then
            if (x ~= nil) then
                mpi_copy(x, C);
            end
            if (y ~= nil) then
                mpi_copy(y, D);
            end
            if (g ~= nil) then
                mpi_mul(gx, v, g);
            end
            return;
        end
    end
end
local assert_mpi_invmod = ASSERTION_GEN("mpi_invmod", "a, m, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('m', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'),
    ASSERT_PARAM_VAL('a', 'mpi_cmp_z(@PARAM@) == 0', ' must not be zero.'),
    ASSERT_PARAM_VAL('m', 'mpi_cmp_z(@PARAM@) == 0', ' must not be zero.'));
function mpi_invmod(a, m, c)
    assert(assert_mpi_invmod(a, m, c));
    if c == nil then
        c = a;
    end
    local g = {0, sign='+'};
    local x = {0, sign='+'};
    local sa;
    sa = a.sign or '+';
    mpi_xgcd(a, m, g, x, nil);
    if (mpi_cmp_d(g, 1) ~= MPI_EQ) then
        return false;
    end
    mpi_mod(x, m, c);
    c.sign = sa;
    return true;
end
local assert_mpi_addmod = ASSERTION_GEN("mpi_addmod", "a, b, m, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'),
    ASSERT_PARAM_TYPE('m', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_addmod(a, b, m, c)
    assert(assert_mpi_addmod(a, b, m, c));
    if c == nil then
        c = a;
    end
    local temp = {0, sign='+'};
    mpi_copy(temp, m);
    if c ~= a then
        mpi_copy(c, a);
    end
    mpi_add(c, b, c);
    mpi_mod(c, temp, c);
end
local assert_mpi_submod = ASSERTION_GEN("mpi_submod", "a, b, m, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'),
    ASSERT_PARAM_TYPE('m', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_submod(a, b, m, c)
    assert(assert_mpi_submod(a, b, m, c));
    local temp = {0, sign='+'};
    mpi_copy(temp, m);
    if c == nil then
        c = a;
    end
    if c ~= a then
        mpi_copy(c, a);
    end
    mpi_sub(c, b, c);
    mpi_mod(c, temp, c);
end
local assert_mpi_mulmod = ASSERTION_GEN("mpi_mulmod", "a, b, m, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'),
    ASSERT_PARAM_TYPE('m', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_mulmod(a, b, m, c)
    assert(assert_mpi_mulmod(a, b, m, c));
    local temp = {0, sign='+'};
    mpi_copy(temp, m);
    if c == nil then
        c = a;
    end
    if c ~= a then
        mpi_copy(c, a);
    end
    mpi_mul(c, b, c);
    mpi_mod(c, temp, c);
end
local assert_mpi_sqrmod = ASSERTION_GEN("mpi_sqrmod", "a, b, m, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('m', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_sqrmod(a, m, c)
    assert(assert_mpi_sqrmod(a, m, c))
    local temp = {0, sign='+'};
    mpi_copy(temp, m);
    if c == nil then
        c = a;
    end
    if c ~= a then
        mpi_copy(c, a);
    end
    mpi_sqr(c, c);
    mpi_mod(c, temp, c);
end
local assert_mpi_exptmod = ASSERTION_GEN("mpi_exptmod", "a, b, m, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('b', 'table'),
    ASSERT_PARAM_TYPE('m', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_exptmod(a, b, m, c)
    assert(assert_mpi_exptmod(a, b, m, c));
    local s = {0, sign = '+'};
    local x = {0, sign = '+'};
    local mu = {0, sign = '+'};
    local ub = #b;
    local d;
    if (mpi_cmp_z(b) < 0 or mpi_cmp_z(m) <= 0) then
        error("mpi_exptmod: divide by zero.");
    end
    mpi_copy(x, a);
    mpi_mod(x, m, x);
    mpi_set_d(s, 1);
    s_mpi_add_d(mu, 1); 
    s_mpi_lshd(mu, 2 * #m);
    mpi_div(mu, m, mu, nil);
    for ix=1,ub - 1 do
        d = b[ix];
        for bit=1,MPI_DIGIT_BIT do
            if ((d % 2) == 1) then
                s_mpi_mul(s, x);
	            s_mpi_reduce(s, m, mu);
            end
            d = bit_rshift(d, 1);
            s_mpi_sqr(x);
            s_mpi_reduce(x, m, mu);
        end
    end
    d = b[#b];
    bit = 1;
    while (d > 0) do
        if ((d % 2) == 1) then
            s_mpi_mul(s, x);
            s_mpi_reduce(s, m, mu);
        end
        d = bit_rshift(d, 1);
        s_mpi_sqr(x);
        s_mpi_reduce(x, m, mu);
        bit = bit + 1;
    end
    mpi_copy(c, s);
end
local assert_mpi_exptmod_d = ASSERTION_GEN("mpi_exptmod_d", "a, d, m, c",
    ASSERT_PARAM_TYPE('a', 'table'),
    ASSERT_PARAM_TYPE('d', 'number'),
    ASSERT_PARAM_TYPE('m', 'table'),
    ASSERT_PARAM_TYPE('c', 'table', 'nil'));
function mpi_exptmod_d(a, d, m, c)
    assert(assert_mpi_exptmod_d(a, d, m, c));
    local s = {0, sign = '+'};
    local x = {0, sign = '+'};
    mpi_copy(x, a);
    mpi_set_d(s, 1);
    while (d > 0) do
        if ((d % 2) == 1) then
            s_mpi_mul(s, x);
            mpi_mod(s, m, s);
        end
        d = bit_rshift(d, 1);
        s_mpi_sqr(x);
        mpi_mod(x, m, x);
    end
    mpi_copy(c, s);
end
local assert_mpi_set = ASSERTION_GEN("mpi_set", "mpi, num, base",
    ASSERT_PARAM_TYPE('mpi', 'table'),
    ASSERT_PARAM_TYPE('num', 'number', 'table', 'string', 'nil'),
    ASSERT_PARAM_TYPE('base', 'number', 'nil'));
function mpi_set(mpi, num, base)
    assert(assert_mpi_set(mpi, num, base));
    local base = base or nil;
    local ix;
    local match;
    local sf;
    if num == nil then
        mpi_zero(mpi);
    elseif type(num) == "table" and #num > 0 then
        mpi_copy(mpi, num);
    elseif type(num) == "string" or type(num) == "number" then
        num = string.gsub(num, " ", "");
        num = string.gsub(num, "_", "");
        mpi_zero(mpi);
        local sign = '+';
        if string.sub(num, 1, 1) == '+' or string.sub(num, 1, 1) == '-' then
            sign = string.sub(num, 1, 1);
            num = string.sub(num, 2);
        else
            sign = '+';
            num = tostring(num);
        end
        num = string.lower(num);
        sf = string.find(num, "%.");
        if sf ~= nil then
           num = string.sub(num, 1, sf - 1);
        end
        if base == nil then
            if string.sub(num, 1, 2) == '0x' then
                base = 16;
            elseif string.sub(num, 1, 2) == '0b' then
                base = 2;
            else
                base = 10;
            end
        end
        local conv = s_mpi_logtab[base];
        if conv == nil then
            error('mpi_set: argument "base" with a value of "' .. base .. '" is not a valid choice, base 2 - 36 supported.');
        end
        if base == 10 then
            sf = string.find(num, "e");
            if sf ~= nil then 
                local e = string.sub(num, sf + 1);
                e = tonumber(e);
                if e ~= nil and e > 0 then 
                    e = tonumber(e);
                else
                    error('mpi_set: argument "num" is not a valid number.');
                end
                num = string.sub(num, 1, sf - 2);
                for i=string.len(num), e do
                    num = num .. "0";
                end
            end
        elseif base == 16 then
            if string.sub(num, 1, 2) == "0x" then
                num = string.sub(num, 3);
            end
        elseif base == 2 then
            if string.sub(num, 1, 2) == "0b" then
                num = string.sub(num, 3);
            end
        end
        num = string.gsub(num, "^0*", "");
        if string.len(num) == 0 then
            num = "0";
        end
        for ix=1,string.len(num) do
            local chr = string.sub(num, ix, ix);
            local val = s_mpi_tovalue(chr, base);
            if val < 0 then
                error("mpi_set: string is not a valid number");
            end
            if conv[2] == 0 then
                s_mpi_mul_d(mpi, base);
            else
                s_mpi_mul_2d(mpi, conv[2]);
            end
            s_mpi_add_d(mpi, val);
        end
        mpi.sign = sign;
        s_mpi_clamp(mpi);
    end
end
local assert_mpi_tostring = ASSERTION_GEN("mpi_tostring", "mpi, num, base",
    ASSERT_PARAM_TYPE('mpi', 'table'),
    ASSERT_PARAM_TYPE('base', 'number', 'nil'));
function mpi_tostring(mpi, base, prefix)
    assert(assert_mpi_tostring(mpi, base, prefix));
    local base = base or 10;
    local prefix = prefix and 1 or 0;
    local temp = {0};
    local conv = s_mpi_logtab[base];
    if conv == nil then
        error("mpi_tostring: base '" .. base .. "' is not a valid choice, only base 2 - 36 supported.")
    end
    if mpi_cmp_z(mpi) == 0 then
        return '0';
    end
    s_mpi_copy(temp, mpi);
    local output = "";
    while mpi_cmp_z(temp) > 0 do
        local val = s_mpi_div_d(temp, base);
        local chr = s_mpi_todigit(val, base);
        output = output .. chr;
    end
    output = string.reverse(output);
    if prefix ~= 0 then
        if base == 16 then
            output = '0x' .. output;
        elseif base == 2 then
            output = '0b' .. output;
        end
    end
    if mpi.sign == '-' then
        output = '-' .. output;
    end
    return output;
end
do
end
