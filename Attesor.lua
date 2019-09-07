local Attesor, Attesor_ST = ...
local L = {};
if GetLocale() == "frFR" then
	L["vu"] = "Attesor vu.";
	L["on"] = "activ\195\169";
	L["off"] = "d\195\169sactiv\195\169";
else
	L["vu"] = "Attesor seen.";
	L["on"] = "activated";
	L["off"] = "deactivated";
end
Attesor_PlayersSeen = {};
Attesor_PlayersCache = {};
Attesor_RealmName  = "Unknown";
Attesor_PlayerName = "Unknown";
Attesor_Enabled = false;
Attesor_Tongue  = "";
Attesor_MAXLEN = 255;
MAX_CHATBUBBLE_WIDTH = 300
local events = {
	CHAT_MSG_SAY = "chatBubbles", CHAT_MSG_YELL = "chatBubbles",
	CHAT_MSG_PARTY = "chatBubblesParty", CHAT_MSG_PARTY_LEADER = "chatBubblesParty",
	CHAT_MSG_MONSTER_SAY = "chatBubbles", CHAT_MSG_MONSTER_YELL = "chatBubbles", CHAT_MSG_MONSTER_PARTY = "chatBubblesParty",
}
local UCommon = { 
	[1] = {'b','r','g','p',},
	[2] = {'rm','lr','gl','gu','gg','mr','ml','rl','hg','xd','lg','mm'},
};
local GCommon = { 
	[1] = {'e','y','o','u',},
	[2] = {'ko','ru','ve','me','re','an','lo','se','ti','lu','ne','va'},
};
local UOrcish = { 
	[1] = {"b","u","r","g","m",},
	[2] = {"lr","gl","mg","mr","ml","rl","ll","lg"},
};
local GOrcish = { 
	[1] = {"g","l","n","o","a",},
	[2] = {"il","no","gi","ag","ha","ko","mu","ka"},
};
function Attesor_StatusMessage(msg)
	DEFAULT_CHAT_FRAME:AddMessage("Attesor : "..msg);
end
function Attesor_OnOff()
	if Attesor_Enabled == false then
		Attesor_Enabled = true
	        Attesor_StatusMessage(L["on"]);
	else
		Attesor_Enabled  = false
	        Attesor_StatusMessage(L["off"]);
	end
end
function Attesor_Init()
	Attesor_Debug = "";
	Attesor_RealmName	= GetRealmName();
	Attesor_PlayerName = UnitName("player");	
	Attesor_ST.UCommon = Attesor_ST.GenerateMaskedWordBase(UCommon)
	Attesor_ST.GCommon = Attesor_ST.GenerateMaskedWordBase(GCommon)
	Attesor_ST.UOrcish = Attesor_ST.GenerateMaskedWordBase(UOrcish)
	Attesor_ST.GOrcish = Attesor_ST.GenerateMaskedWordBase(GOrcish)
	DEFAULT_CHAT_FRAME:AddMessage("Attesor charg\195\169 -- Gibberish(/ag*)");
	SlashCmdList["AttesorSAY"] = Attesor_Say;
	SLASH_AttesorSAY1 = "/ags";
	SlashCmdList["AttesorYELL"] = Attesor_Yell;
	SLASH_AttesorYELL1 = "/agy";
	SlashCmdList["AttesorPARTY"] = Attesor_Party;
	SLASH_AttesorPARTY1 = "/agp";
	SlashCmdList["AttesorRAID"] = Attesor_Raid;
	SLASH_AttesorRAID1 = "/agr";
	SlashCmdList["AttesorGUILD"] = Attesor_Guild;
	SLASH_AttesorGUILD1 = "/agg";
	SlashCmdList["AttesorSAYT"] = Attesor_SayT;
	SLASH_AttesorSAYT1 = "/ats";
	SlashCmdList["AttesorYELLT"] = Attesor_YellT;
	SLASH_AttesorYELLT1 = "/aty";
	SlashCmdList["AttesorPARTYT"] = Attesor_PartyT;
	SLASH_AttesorPARTYT1 = "/atp";
	SlashCmdList["AttesorSELECT"] = Attesor_Select;
	SLASH_AttesorSELECT1 = "/atc";
	SlashCmdList["AttesorCLEAR"] = Attesor_ClearPlayersSeen;
	SLASH_AttesorCLEAR1 = "/agc";
	SlashCmdList["AttesorENABLE"] = Attesor_OnOff;
	SLASH_AttesorENABLE1 = "/agt";
	Attesor_Old_ChatFrame_OnEvent = ChatFrame_OnEvent;
	ChatFrame_OnEvent = Attesor_ChatFrame_OnEvent;
	Attesor_Old_SendChatMessage = SendChatMessage;
	SendChatMessage = Attesor_SendChatMessage;
end
function Attesor__OnShow()
	name,unit = GameTooltip:GetUnit()
	if unit then
		name,server = UnitFullName(unit)
		if server then name = name..'-'..server end
		if ( name and Attesor_PlayersSeen[name] ) then
			GameTooltip:AddLine(L["vu"], 1.0, 0.0, 0.5);
			GameTooltip:Show();
		end
	end
end
function Attesor_Select(arg1) 
	Attesor_Tongue = arg1 ;  
end
function Attesor_Say(arg1) 
	Attesor_SendBable(arg1,"SAY");
end
function Attesor_SayT(arg1) 
	SendChatMessage( arg1, "SAY", Attesor_Tongue );  
end
function Attesor_Yell(arg1) 
	Attesor_SendBable(arg1,"YELL");
end
function Attesor_YellT(arg1) 
	SendChatMessage( arg1, "YELL", Attesor_Tongue );  
end
function Attesor_Party(arg1) 
	Attesor_SendBable(arg1,"PARTY");
end
function Attesor_PartyT(arg1) 
	SendChatMessage( arg1, "PARTY", Attesor_Tongue );  
end
function Attesor_Raid(arg1) 
	Attesor_SendBable(arg1,"RAID");
end
function Attesor_Guild(arg1) 
	Attesor_SendBable(arg1,"GUILD");
end
function Attesor_ClearPlayersSeen()
	Attesor_PlayersSeen = {};
end
function Attesor_ItemLinkFromId(id) 
	local itemName, itemLink = GetItemInfo(id);
	if (itemLink) then
		return itemLink;
	else
		return "|cff00dddd[Lien incorrect]|r";
	end
end
function EncodeBable(msg)
	local Attesor_Language, Attesor_LanguageNb  = GetDefaultLanguage();
	if Attesor_LanguageNb == 7 then return Attesor_ST.ChiffreMsg(msg,Attesor_ST.UCommon) end
	if Attesor_LanguageNb == 1 then return Attesor_ST.ChiffreMsg(msg,Attesor_ST.UOrcish) end
end
local function CutBable(msg)
	if string.len(msg) > Attesor_MAXLEN then
		for i = 0,Attesor_MAXLEN do
			local y = Attesor_MAXLEN-i
			local c = string.sub(msg, y,y);
			if c == " " then
				local fp = string.sub(msg, 1,y-1);
				local lp = string.sub(msg, y);
				local ret = CutBable(lp)
				table.insert(ret,fp)
				return ret
			end
		end
	else
		return {msg}
	end
end
function Attesor_SendBable(message,destination)
	if (not destination) then destination = "SAY" end
	local name = UnitName("target");
	if (not name) then
		name = "<pas de cible>";
	end
	local fname = UnitName("focus");
	if (not fname) then
		fname = "<pas de focalisation>";
	end
	message = string.gsub(message,"%%t",name);
	message = string.gsub(message,"%%f",fname);
	message = string.gsub(message,"%%(%d+)",Attesor_ItemLinkFromId);
	bable = EncodeBable('[AR] '..message);
	bable = CutBable(bable);
	for i = 1, table.maxn(bable) do
		Attesor_Old_SendChatMessage(bable[i],destination);
	end
end
function DecodeBable(msg,language)
	local Attesor_Language, Attesor_LanguageNb  = GetDefaultLanguage();
	if language == Attesor_Language or language == "" then
		if Attesor_LanguageNb == 7 then return Attesor_ST.DechiffreMsg(msg,Attesor_ST.UCommon) end
		if Attesor_LanguageNb == 1 then return Attesor_ST.DechiffreMsg(msg,Attesor_ST.UOrcish) end
	else
		if Attesor_LanguageNb == 7 then return Attesor_ST.DechiffreMsg(msg,Attesor_ST.GOrcish) end
		if Attesor_LanguageNb == 1 then return Attesor_ST.DechiffreMsg(msg,Attesor_ST.GCommon) end
	end
	return nil
end
function Attesor_SendChatMessage(msg, chattype, language, channel)
	if Attesor_Enabled == true and (chattype == "SAY" or chattype == "YELL" ) then
		Attesor_SendBable(msg, chattype);
	else 
		Attesor_Old_SendChatMessage(msg, chattype, language, channel);
	end
end
function Attesor_ChatFrame_OnEvent(self,event,arg1,arg2,arg3,arg4,arg5,...)
	if ((event == "CHAT_MSG_SAY") or (event == "CHAT_MSG_YELL") or (event == "CHAT_MSG_PARTY") or  (event == "CHAT_MSG_PARTY_LEADER") or (event == "CHAT_MSG_RAID") or (event == "CHAT_MSG_RAID_LEADER") or (event == "CHAT_MSG_GUILD"))
	then
		local msg,sender,language = arg1,arg2,arg3;
		if  ( not Attesor_PlayersCache[sender] ) then Attesor_PlayersCache[sender] = {} end
		fc = string.sub(msg, 1,1);
		if fc == " " then
			local maxn = table.maxn(Attesor_PlayersCache[sender])
			if maxn > 0 then
				last_event = Attesor_PlayersCache[sender][maxn][3];
				last_msg = Attesor_PlayersCache[sender][maxn][4];
				if ((last_event == event) and (last_msg == msg)) then
					return Attesor_Old_ChatFrame_OnEvent(self,event,arg1,arg2,arg3,arg4,arg5,...);
				end
			end
			table.insert(Attesor_PlayersCache[sender],{GetTime()*1000,self,event,arg1,arg2,arg3,arg4,arg5,...});
			return
		else
			pmsg= ""
			for i = 1, table.maxn(Attesor_PlayersCache[sender]) do
				pmsg = Attesor_PlayersCache[sender][i][4] .. pmsg;
			end
			msg = msg .. pmsg;
			Attesor_PlayersCache[sender] = {};
		end
		local decode = DecodeBable(msg,language);
		if (decode ~= nil) and (string.sub(decode,1,4) == '[AR]') then 
			if (event == "CHAT_MSG_SAY") or (event == "CHAT_MSG_YELL") or (event == "CHAT_MSG_PARTY") or (event == "CHAT_MSG_PARTY_LEADER") then 
				FindAndUpdateFrame(event, arg1, decode, sender)
			end
			arg1 = "" .. decode;
			if ( not Attesor_PlayersSeen[sender] ) then
				Attesor_PlayersSeen[sender] = true
			end
		end
	end
	return Attesor_Old_ChatFrame_OnEvent(self,event,arg1,arg2,arg3,arg4,arg5,...);
end
local function Attesor_OnUpdate(self,elapsed)  
	local ctime = GetTime()*1000;
	local bandwidthIn, bandwidthOut, latencyHome, latencyWorld = GetNetStats(); 
	for k,v in pairs(Attesor_PlayersCache) do
		local maxn = table.maxn(Attesor_PlayersCache[k]);
		for i = 0, maxn-1 do
			stime = Attesor_PlayersCache[k][maxn-i][1];
			if ctime > stime+min(latencyWorld*4,100) then
				local tab = Attesor_PlayersCache[k][maxn-i];
				table.remove(tab,1);
				Attesor_Old_ChatFrame_OnEvent(unpack(tab));
				table.remove(Attesor_PlayersCache[k],maxn);
			end
		end
	end
end 
Attesor_Frame = CreateFrame("frame")  
Attesor_Frame:SetScript("OnUpdate", Attesor_OnUpdate)  
local function UpdateFrame(frame, sender, msg)
	for i = 1, select("#", frame:GetRegions()) do
		local region = select(i, frame:GetRegions())
		if region:GetObjectType() == "FontString" then
			frame.text = region
		end
	end
	frame.text:SetText(msg)
	local mcbw = math.max(frame:GetWidth(), MAX_CHATBUBBLE_WIDTH)
	frame.text:SetWidth(math.min(frame.text:GetStringWidth(), mcbw - 14))
end
local function FindFrame(msg)
	local frames = C_ChatBubbles.GetAllChatBubbles()
	for index = 1, #frames do
		local frame = frames[index]
		for i = 1, frame:GetNumRegions() do
			local region = select(i, frame:GetRegions())
			if region:GetObjectType() == "FontString" and region:GetText() == msg then
				return frame
			end
		end
	end
end
function FindAndUpdateFrame(event, msg, dmsg, sender)
	if GetCVarBool(events[event]) then
		local f = CreateFrame("Frame")
		f.elapsed = 0
		f:SetScript("OnUpdate", function(self, elapsed)
			self.elapsed = self.elapsed + elapsed
			local frame = FindFrame(msg)
			if frame or self.elapsed > 0.3 then
				f:SetScript("OnUpdate", nil)
				if frame then UpdateFrame(frame, sender, dmsg) end
			end
		end)
	end
end
