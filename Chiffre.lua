local Attesor, Attesor_ST = ...
if Attesor_ST == nil then
	Attesor_ST = {}
end
if (mpi == nil) and (require ~= nil) then
	require("mpi");
end
local rawcharset = {' ','\167','\195','\194','\169','\168','\160','\162','\128','\153','\226','\197','\146','\175','\185','e','t','a','o','i','n','s','r','h','l','d','c','u','m','f','p','g','w','y','b','v','k','x','j','q','z','E','T','A','O','I','N','S','R','H','L','D','C','U','M','F','P','G','W','Y','B','V','K','X','J','Q','Z','ç','.',',','?','!','1','2','3','4','5','6','7','8','9','0','=',"'",'"',':','(',')','^','*','<','>','@','$','%','/','~','-','+','|','[',']','&','_',';',"\\"}
Attesor_ST.rawcharset = rawcharset
local function PrintTable(tab)
	str = "["
	for i = 0, table.maxn(tab) do
		str = str .. tostring(tab[i]) .. ","
	end
	str = str .. "]"
	print(str)
end
Attesor_ST.PrintTable = PrintTable
local function Split(inputstr, sep)
        if sep == nil then
                sep = "%s"
        end
        t={} ; i=1
        for str in string.gmatch(inputstr, "([^"..sep.."]+)") do
                t[i] = str
                i = i + 1
        end
        return t
end
local function MaskWord(word,mask)
	local ret = ''
	if (#word ~=  #mask) then
		return
	end;
	for n=1,string.len(word) do
		if string.match(string.sub(mask,n,n),"%u") then
			ret = ret .. string.upper(string.sub(word,n,n))
		else
			ret = ret .. string.sub(word,n,n)
		end;
	end;	
	return ret
end
local function GenerateMasksByWordLen(wordlen)
	masksX = {[1]={'x','X'}}
	for i = 2, wordlen do
		masksX[i] = {}
		for y = 1, #masksX[i-1] do
			table.insert(masksX[i],masksX[i-1][y]..masksX[1][1])
			table.insert(masksX[i],masksX[i-1][y]..masksX[1][2])
		end
	end
	return masksX
end
local function VerifyWords(words)
	local wordslen = #words
	local wordlen = #words[1]
	for i = 2,  wordslen-1 do
		if wordlen ~= #words[i] then error("invalid words") end;
	end;	
	return words,wordslen,wordlen
end
local function GenerateMaskedWordBase(blwords)
	local base = {base_n=0}
	base.enc = {}
	base.dec = {}
	local masks = GenerateMasksByWordLen(#blwords)
	for x = 1, #blwords do
		local words,wordslen,wordlen = VerifyWords(blwords[x])
		for i = 0, wordslen-1 do
			local masklen = #masks[wordlen]
			for y = 0, masklen-1 do
				local masked = MaskWord(words[i+1],masks[wordlen][y+1])
				base.enc[base.base_n] = masked
				base.dec[masked] = base.base_n
				base.base_n = base.base_n + 1
			end;
		end;
	end;
	return base
end
local function GenerateCharsetBase(rawcharset)
	local rawcharset_size = #rawcharset
	local char_base = {base_n=0,enc={},dec={}}
	for i = 0, rawcharset_size-1 do
		char_base.enc[i] = rawcharset[i+1]
		char_base.dec[rawcharset[i+1]] = i
		char_base.base_n = char_base.base_n + 1 
	end
	return char_base
end
local function TextToArray(str)
	ret = {}
	for i = 1,string.len(str) do
		local c = string.sub(str, i, i)
		table.insert(ret,i,c)
	end
	return ret
end
local function ArrayToMPI(tab, base)
	local ret = {0, sign = '+'};
	if type(tab) == 'string' then
		tab = TextToArray(tab)
	end
	local tabmaxn = #tab
	if type(tab) == 'table' then
		for i = 1, tabmaxn do
			local j = tabmaxn-i
			local mpx = {base.base_n, sign = '+'};
			mpi_expt_d(mpx, j, mpx)
			number = tonumber(base.dec[tab[i]])
			if number == nil then return false, tab[i] end
			mpi_mul_d(mpx, number)
			mpi_add(ret,mpx)
		end
		return true, ret
	end
end
local function MPIToArray(mpx, base)
	ret = {}
	while (mpi_cmp_z(mpx) ~= 0) do
		local base_n = base.base_n
		mpt = {0, sign = '+'};
		rem = mpi_div_d(mpx, base_n, mpt)
		mpx = mpt
		to_insert = base.enc[tostring(rem)]
		if to_insert == nil then to_insert = base.enc[rem] end
		table.insert(ret,1,to_insert)
	end
	return ret
end
local function JoinTable(tab,sep)
	str = ""
	if sep then
		sep = tostring(sep)
		for i = 1, table.maxn(tab) do
			str = str .. tostring(tab[i]..sep)
		end
	else
		for i = 1, table.maxn(tab) do
			str = str .. tostring(tab[i])
		end
	end
	return str
end
local function EscapeText(str)
	local ret = ''
	local k = 0
	for i = 1,string.len(str) do
		local c = string.sub(str, i, i)
		if c ~= ' ' then k = k + 1  else k = 0 end
		if k == 12 then
			k = 0
			ret = ret .. c .. ' '
		else
			ret = ret .. c
		end
	end
	return ret
end
local function UnescapeText(str)
	local ret = ''
	local k = 0
	for i = 1,string.len(str) do
		local c = string.sub(str, i, i)
		if k == 12 and c == ' ' then
			k = 0
		else
			ret = ret .. c
			if c ~= ' ' then k = k + 1  else k = 0 end
		end
	end
	return ret
end
charset = GenerateCharsetBase(rawcharset)
local function ChiffreMsg(msg,base)
	local ok, lint_msg =  ArrayToMPI(msg, charset)
	if ok == false then return nil end
	local tabX_msg = MPIToArray(lint_msg, base)
	local str_msg = JoinTable(tabX_msg,' ')
	return str_msg
end
Attesor_ST.ChiffreMsg = ChiffreMsg
local function DechiffreMsg(msg,base)
	local tab3_msg = Split(msg)
	local ok, int3_msg = ArrayToMPI(tab3_msg, base)
	if ok == false then return nil end
	local txt_msg = MPIToArray(int3_msg,charset)
	local txt_msg = JoinTable(txt_msg)
	return txt_msg
end
Attesor_ST.DechiffreMsg = DechiffreMsg
Attesor_ST.GenerateMaskedWordBase = GenerateMaskedWordBase