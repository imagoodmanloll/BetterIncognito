-- not my base but who cares

local ContentProvider = game:GetService("ContentProvider")
local HttpService = game:GetService("HttpService")

-- UTILS

local core_gui = game:GetService("CoreGui")
local core_packages = game:GetService("CorePackages")


local load = coroutine.wrap(function()
	local compile = coroutine.wrap(function()
		local luaZ = {}
		local luaY = {}
		local luaX = {}
		local luaP = {}
		local luaU = {}
		local luaK = {}
		local size_size_t = 8

		local function lua_assert(test)
			if not test then error("assertion failed!") end
		end

		function luaZ:make_getS(buff)
			local b = buff
			return function()
				if not b then return nil end
				local data = b
				b = nil
				return data
			end
		end

		function luaZ:make_getF(source)
			local LUAL_BUFFERSIZE = 512
			local pos = 1

			return function() -- chunk reader anonymous function here
				local buff = source:sub(pos, pos + LUAL_BUFFERSIZE - 1)
				pos = math.min(#source + 1, pos + LUAL_BUFFERSIZE)
				return buff
			end
		end

		function luaZ:init(reader, data)
			if not reader then return end
			local z = {}
			z.reader = reader
			z.data = data or ""
			z.name = name
			if not data or data == "" then z.n = 0 else z.n = #data end
			z.p = 0
			return z
		end

		function luaZ:fill(z)
			local buff = z.reader()
			z.data = buff
			if not buff or buff == "" then return "EOZ" end
			z.n, z.p = #buff - 1, 1
			return string.sub(buff, 1, 1)
		end

		function luaZ:zgetc(z)
			local n, p = z.n, z.p + 1
			if n > 0 then
				z.n, z.p = n - 1, p
				return string.sub(z.data, p, p)
			else
				return self:fill(z)
			end
		end

		luaX.RESERVED = [[
TK_AND and
TK_BREAK break
TK_DO do
TK_ELSE else
TK_ELSEIF elseif
TK_END end
TK_FALSE false
TK_FOR for
TK_FUNCTION function
TK_IF if
TK_IN in
TK_LOCAL local
TK_NIL nil
TK_NOT not
TK_OR or
TK_REPEAT repeat
TK_RETURN return
TK_THEN then
TK_TRUE true
TK_UNTIL until
TK_WHILE while
TK_CONCAT ..
TK_DOTS ...
TK_EQ ==
TK_GE >=
TK_LE <=
TK_NE ~=
TK_NAME <name>
TK_NUMBER <number>
TK_STRING <string>
TK_EOS <eof>]]

		luaX.MAXSRC = 80
		luaX.MAX_INT = 2147483645
		luaX.LUA_QS = "'%s'"
		luaX.LUA_COMPAT_LSTR = 1

		function luaX:init()
			local tokens, enums = {}, {}
			for v in string.gmatch(self.RESERVED, "[^\n]+") do
				local _, _, tok, str = string.find(v, "(%S+)%s+(%S+)")
				tokens[tok] = str
				enums[str] = tok
			end
			self.tokens = tokens
			self.enums = enums
		end

		function luaX:chunkid(source, bufflen)
			local out
			local first = string.sub(source, 1, 1)
			if first == "=" then
				out = string.sub(source, 2, bufflen)
			else
				if first == "@" then
					source = string.sub(source, 2)
					bufflen = bufflen - #" '...' "
					local l = #source
					out = ""
					if l > bufflen then
						source = string.sub(source, 1 + l - bufflen) 
						out = out.."..."
					end
					out = out..source
				else 
					local len = string.find(source, "[\n\r]") 
					len = len and (len - 1) or #source
					bufflen = bufflen - #(" [string \"...\"] ")
					if len > bufflen then len = bufflen end
					out = "[string \""
					if len < #source then
						out = out..string.sub(source, 1, len).."..."
					else
						out = out..source
					end
					out = out.."\"]"
				end
			end
			return out
		end

		function luaX:token2str(ls, token)
			if string.sub(token, 1, 3) ~= "TK_" then
				if string.find(token, "%c") then
					return string.format("char(%d)", string.byte(token))
				end
				return token
			else
				return self.tokens[token]
			end
		end

		function luaX:lexerror(ls, msg, token)
			local function txtToken(ls, token)
				if token == "TK_NAME" or
					token == "TK_STRING" or
					token == "TK_NUMBER" then
					return ls.buff
				else
					return self:token2str(ls, token)
				end
			end
			local buff = self:chunkid(ls.source, self.MAXSRC)
			local msg = string.format("%s:%d: %s", buff, ls.linenumber, msg)
			if token then
				msg = string.format("%s near "..self.LUA_QS, msg, txtToken(ls, token))
			end
			error(msg)
		end

		function luaX:syntaxerror(ls, msg)
			self:lexerror(ls, msg, ls.t.token)
		end

		function luaX:currIsNewline(ls)
			return ls.current == "\n" or ls.current == "\r"
		end

		function luaX:inclinenumber(ls)
			local old = ls.current
			self:nextc(ls)
			if self:currIsNewline(ls) and ls.current ~= old then
				self:nextc(ls)
			end
			ls.linenumber = ls.linenumber + 1
			if ls.linenumber >= self.MAX_INT then
				self:syntaxerror(ls, "chunk has too many lines")
			end
		end

		function luaX:setinput(L, ls, z, source)
			if not ls then ls = {} end
			if not ls.lookahead then ls.lookahead = {} end
			if not ls.t then ls.t = {} end
			ls.decpoint = "."
			ls.L = L
			ls.lookahead.token = "TK_EOS" 
			ls.z = z
			ls.fs = nil
			ls.linenumber = 1
			ls.lastline = 1
			ls.source = source
			self:nextc(ls)
		end

		function luaX:check_next(ls, set)
			if not string.find(set, ls.current, 1, 1) then
				return false
			end
			self:save_and_next(ls)
			return true
		end

		function luaX:next(ls)
			ls.lastline = ls.linenumber
			if ls.lookahead.token ~= "TK_EOS" then
				-- this must be copy-by-value
				ls.t.seminfo = ls.lookahead.seminfo 
				ls.t.token = ls.lookahead.token
				ls.lookahead.token = "TK_EOS" 
			else
				ls.t.token = self:llex(ls, ls.t) 
			end
		end

		function luaX:lookahead(ls)

			ls.lookahead.token = self:llex(ls, ls.lookahead)
		end

		function luaX:nextc(ls)
			local c = luaZ:zgetc(ls.z)
			ls.current = c
			return c
		end

		function luaX:save(ls, c)
			local buff = ls.buff
			ls.buff = buff..c
		end

		function luaX:save_and_next(ls)
			self:save(ls, ls.current)
			return self:nextc(ls)
		end

		function luaX:str2d(s)
			local result = tonumber(s)
			if result then return result end
			if string.lower(string.sub(s, 1, 2)) == "0x" then
				result = tonumber(s, 16)
				if result then return result end
			end
			return nil
		end

		function luaX:buffreplace(ls, from, to)
			local result, buff = "", ls.buff
			for p = 1, #buff do
				local c = string.sub(buff, p, p)
				if c == from then c = to end
				result = result..c
			end
			ls.buff = result
		end

		function luaX:trydecpoint(ls, Token)
			local old = ls.decpoint
			self:buffreplace(ls, old, ls.decpoint)
			local seminfo = self:str2d(ls.buff)
			Token.seminfo = seminfo
			if not seminfo then
				self:buffreplace(ls, ls.decpoint, ".")
				self:lexerror(ls, "malformed number", "TK_NUMBER")
			end
		end

		function luaX:read_numeral(ls, Token)
			repeat
				self:save_and_next(ls)
			until string.find(ls.current, "%D") and ls.current ~= "."
			if self:check_next(ls, "Ee") then
				self:check_next(ls, "+-")
			end
			while string.find(ls.current, "^%w$") or ls.current == "_" do
				self:save_and_next(ls)
			end
			self:buffreplace(ls, ".", ls.decpoint)
			local seminfo = self:str2d(ls.buff)
			Token.seminfo = seminfo
			if not seminfo then
				self:trydecpoint(ls, Token) 
			end
		end

		function luaX:skip_sep(ls)
			local count = 0
			local s = ls.current
			self:save_and_next(ls)
			while ls.current == "=" do
				self:save_and_next(ls)
				count = count + 1
			end
			return (ls.current == s) and count or (-count) - 1
		end

		function luaX:read_long_string(ls, Token, sep)
			local cont = 0
			self:save_and_next(ls)
			if self:currIsNewline(ls) then
				self:inclinenumber(ls)
			end
			while true do
				local c = ls.current
				if c == "EOZ" then
					self:lexerror(ls, Token and "unfinished long string" or
						"unfinished long comment", "TK_EOS")
				elseif c == "[" then
					if self.LUA_COMPAT_LSTR then
						if self:skip_sep(ls) == sep then
							self:save_and_next(ls)
							cont = cont + 1
							if self.LUA_COMPAT_LSTR == 1 then
								if sep == 0 then
									self:lexerror(ls, "nesting of [[...]] is deprecated", "[")
								end
							end
						end
					end
				elseif c == "]" then
					if self:skip_sep(ls) == sep then
						self:save_and_next(ls)
						if self.LUA_COMPAT_LSTR and self.LUA_COMPAT_LSTR == 2 then
							cont = cont - 1
							if sep == 0 and cont >= 0 then break end
						end
						break
					end
				elseif self:currIsNewline(ls) then
					self:save(ls, "\n")
					self:inclinenumber(ls)
					if not Token then ls.buff = "" end
				else
					if Token then
						self:save_and_next(ls)
					else
						self:nextc(ls)
					end
				end
			end
			if Token then
				local p = 3 + sep
				Token.seminfo = string.sub(ls.buff, p, -p)
			end
		end

		function luaX:read_string(ls, del, Token)
			self:save_and_next(ls)
			while ls.current ~= del do
				local c = ls.current
				if c == "EOZ" then
					self:lexerror(ls, "unfinished string", "TK_EOS")
				elseif self:currIsNewline(ls) then
					self:lexerror(ls, "unfinished string", "TK_STRING")
				elseif c == "\\" then
					c = self:nextc(ls)
					if self:currIsNewline(ls) then 
						self:save(ls, "\n")
						self:inclinenumber(ls)
					elseif c ~= "EOZ" then
						local i = string.find("abfnrtv", c, 1, 1)
						if i then
							self:save(ls, string.sub("\a\b\f\n\r\t\v", i, i))
							self:nextc(ls)
						elseif not string.find(c, "%d") then
							self:save_and_next(ls)
						else
							c, i = 0, 0
							repeat
								c = 10 * c + ls.current
								self:nextc(ls)
								i = i + 1
							until i >= 3 or not string.find(ls.current, "%d")
							if c > 255 then
								self:lexerror(ls, "escape sequence too large", "TK_STRING")
							end
							self:save(ls, string.char(c))
						end
					end
				else
					self:save_and_next(ls)
				end
			end
			self:save_and_next(ls)
			Token.seminfo = string.sub(ls.buff, 2, -2)
		end

		function luaX:llex(ls, Token)
			ls.buff = ""
			while true do
				local c = ls.current
				if self:currIsNewline(ls) then
					self:inclinenumber(ls)
				elseif c == "-" then
					c = self:nextc(ls)
					if c ~= "-" then return "-" end
					local sep = -1
					if self:nextc(ls) == '[' then
						sep = self:skip_sep(ls)
						ls.buff = ""
					end
					if sep >= 0 then
						self:read_long_string(ls, nil, sep)
						ls.buff = ""
					else
						while not self:currIsNewline(ls) and ls.current ~= "EOZ" do
							self:nextc(ls)
						end
					end
				elseif c == "[" then
					local sep = self:skip_sep(ls)
					if sep >= 0 then
						self:read_long_string(ls, Token, sep)
						return "TK_STRING"
					elseif sep == -1 then
						return "["
					else
						self:lexerror(ls, "invalid long string delimiter", "TK_STRING")
					end
				elseif c == "=" then
					c = self:nextc(ls)
					if c ~= "=" then return "="
					else self:nextc(ls); return "TK_EQ" end
				elseif c == "<" then
					c = self:nextc(ls)
					if c ~= "=" then return "<"
					else self:nextc(ls); return "TK_LE" end
				elseif c == ">" then
					c = self:nextc(ls)
					if c ~= "=" then return ">"
					else self:nextc(ls); return "TK_GE" end
				elseif c == "~" then
					c = self:nextc(ls)
					if c ~= "=" then return "~"
					else self:nextc(ls); return "TK_NE" end
				elseif c == "\"" or c == "'" then
					self:read_string(ls, c, Token)
					return "TK_STRING"
				elseif c == "." then
					c = self:save_and_next(ls)
					if self:check_next(ls, ".") then
						if self:check_next(ls, ".") then
							return "TK_DOTS"
						else return "TK_CONCAT"
						end
					elseif not string.find(c, "%d") then
						return "."
					else
						self:read_numeral(ls, Token)
						return "TK_NUMBER"
					end
				elseif c == "EOZ" then
					return "TK_EOS"
				else
					if string.find(c, "%s") then
						self:nextc(ls)
					elseif string.find(c, "%d") then
						self:read_numeral(ls, Token)
						return "TK_NUMBER"
					elseif string.find(c, "[_%a]") then
						repeat
							c = self:save_and_next(ls)
						until c == "EOZ" or not string.find(c, "[_%w]")
						local ts = ls.buff
						local tok = self.enums[ts]
						if tok then return tok end
						Token.seminfo = ts
						return "TK_NAME"
					else
						self:nextc(ls)
						return c
					end
				end
			end
		end

		luaP.OpMode = { iABC = 0, iABx = 1, iAsBx = 2 }

		luaP.SIZE_C  = 9
		luaP.SIZE_B  = 9
		luaP.SIZE_Bx = luaP.SIZE_C + luaP.SIZE_B
		luaP.SIZE_A  = 8

		luaP.SIZE_OP = 6

		luaP.POS_OP = 0
		luaP.POS_A  = luaP.POS_OP + luaP.SIZE_OP
		luaP.POS_C  = luaP.POS_A + luaP.SIZE_A
		luaP.POS_B  = luaP.POS_C + luaP.SIZE_C
		luaP.POS_Bx = luaP.POS_C

		luaP.MAXARG_Bx  = math.ldexp(1, luaP.SIZE_Bx) - 1
		luaP.MAXARG_sBx = math.floor(luaP.MAXARG_Bx / 2)

		luaP.MAXARG_A = math.ldexp(1, luaP.SIZE_A) - 1
		luaP.MAXARG_B = math.ldexp(1, luaP.SIZE_B) - 1
		luaP.MAXARG_C = math.ldexp(1, luaP.SIZE_C) - 1

		function luaP:GET_OPCODE(i) return self.ROpCode[i.OP] end
		function luaP:SET_OPCODE(i, o) i.OP = self.OpCode[o] end

		function luaP:GETARG_A(i) return i.A end
		function luaP:SETARG_A(i, u) i.A = u end

		function luaP:GETARG_B(i) return i.B end
		function luaP:SETARG_B(i, b) i.B = b end

		function luaP:GETARG_C(i) return i.C end
		function luaP:SETARG_C(i, b) i.C = b end

		function luaP:GETARG_Bx(i) return i.Bx end
		function luaP:SETARG_Bx(i, b) i.Bx = b end

		function luaP:GETARG_sBx(i) return i.Bx - self.MAXARG_sBx end
		function luaP:SETARG_sBx(i, b) i.Bx = b + self.MAXARG_sBx end

		function luaP:CREATE_ABC(o,a,b,c)
			return {OP = self.OpCode[o], A = a, B = b, C = c}
		end

		function luaP:CREATE_ABx(o,a,bc)
			return {OP = self.OpCode[o], A = a, Bx = bc}
		end

		function luaP:CREATE_Inst(c)
			local o = c % 64
			c = (c - o) / 64
			local a = c % 256
			c = (c - a) / 256
			return self:CREATE_ABx(o, a, c)
		end

		function luaP:Instruction(i)
			if i.Bx then
				i.C = i.Bx % 512
				i.B = (i.Bx - i.C) / 512
			end
			local I = i.A * 64 + i.OP
			local c0 = I % 256
			I = i.C * 64 + (I - c0) / 256
			local c1 = I % 256
			I = i.B * 128 + (I - c1) / 256
			local c2 = I % 256
			local c3 = (I - c2) / 256
			return string.char(c0, c1, c2, c3)
		end

		function luaP:DecodeInst(x)
			local byte = string.byte
			local i = {}
			local I = byte(x, 1)
			local op = I % 64
			i.OP = op
			I = byte(x, 2) * 4 + (I - op) / 64
			local a = I % 256
			i.A = a
			I = byte(x, 3) * 4 + (I - a) / 256
			local c = I % 512
			i.C = c
			i.B = byte(x, 4) * 2 + (I - c) / 512
			local opmode = self.OpMode[tonumber(string.sub(self.opmodes[op + 1], 7, 7))]
			if opmode ~= "iABC" then
				i.Bx = i.B * 512 + i.C
			end
			return i
		end

		luaP.BITRK = math.ldexp(1, luaP.SIZE_B - 1)

		function luaP:ISK(x) return x >= self.BITRK end

		function luaP:INDEXK(r) return x - self.BITRK end

		luaP.MAXINDEXRK = luaP.BITRK - 1

		function luaP:RKASK(x) return x + self.BITRK end

		luaP.NO_REG = luaP.MAXARG_A

		luaP.opnames = {} 
		luaP.OpCode = {} 
		luaP.ROpCode = {} 

		local i = 0
		for v in string.gmatch([[
MOVE LOADK LOADBOOL LOADNIL GETUPVAL
GETGLOBAL GETTABLE SETGLOBAL SETUPVAL SETTABLE
NEWTABLE SELF ADD SUB MUL
DIV MOD POW UNM NOT
LEN CONCAT JMP EQ LT
LE TEST TESTSET CALL TAILCALL
RETURN FORLOOP FORPREP TFORLOOP SETLIST
CLOSE CLOSURE VARARG
]], "%S+") do
			local n = "OP_"..v
			luaP.opnames[i] = v
			luaP.OpCode[n] = i
			luaP.ROpCode[i] = n
			i = i + 1
		end
		luaP.NUM_OPCODES = i
		luaP.OpArgMask = { OpArgN = 0, OpArgU = 1, OpArgR = 2, OpArgK = 3 }

		function luaP:getOpMode(m)
			return self.opmodes[self.OpCode[m]] % 4
		end

		function luaP:getBMode(m)
			return math.floor(self.opmodes[self.OpCode[m]] / 16) % 4
		end

		function luaP:getCMode(m)
			return math.floor(self.opmodes[self.OpCode[m]] / 4) % 4
		end

		function luaP:testAMode(m)
			return math.floor(self.opmodes[self.OpCode[m]] / 64) % 2
		end

		function luaP:testTMode(m)
			return math.floor(self.opmodes[self.OpCode[m]] / 128)
		end

		luaP.LFIELDS_PER_FLUSH = 50

		local function opmode(t, a, b, c, m)
			local luaP = luaP
			return t * 128 + a * 64 +
				luaP.OpArgMask[b] * 16 + luaP.OpArgMask[c] * 4 + luaP.OpMode[m]
		end


		luaP.opmodes = {
			opmode(0, 1, "OpArgK", "OpArgN", "iABx"), 
			opmode(0, 1, "OpArgU", "OpArgU", "iABC"),  
			opmode(0, 1, "OpArgR", "OpArgN", "iABC"),    
			opmode(0, 1, "OpArgU", "OpArgN", "iABC"),     
			opmode(0, 1, "OpArgK", "OpArgN", "iABx"),   
			opmode(0, 1, "OpArgR", "OpArgK", "iABC"),     
			opmode(0, 0, "OpArgK", "OpArgN", "iABx"),    
			opmode(0, 0, "OpArgU", "OpArgN", "iABC"),    
			opmode(0, 0, "OpArgK", "OpArgK", "iABC"),     
			opmode(0, 1, "OpArgU", "OpArgU", "iABC"),  
			opmode(0, 1, "OpArgR", "OpArgK", "iABC"),  
			opmode(0, 1, "OpArgK", "OpArgK", "iABC"),   
			opmode(0, 1, "OpArgK", "OpArgK", "iABC"),    
			opmode(0, 1, "OpArgK", "OpArgK", "iABC"),    
			opmode(0, 1, "OpArgK", "OpArgK", "iABC"),   
			opmode(0, 1, "OpArgK", "OpArgK", "iABC"),    
			opmode(0, 1, "OpArgK", "OpArgK", "iABC"),     
			opmode(0, 1, "OpArgR", "OpArgN", "iABC"),    
			opmode(0, 1, "OpArgR", "OpArgN", "iABC"),     
			opmode(0, 1, "OpArgR", "OpArgN", "iABC"),   
			opmode(0, 1, "OpArgR", "OpArgR", "iABC"),
			opmode(0, 0, "OpArgR", "OpArgN", "iAsBx"), 
			opmode(1, 0, "OpArgK", "OpArgK", "iABC"), 
			opmode(1, 0, "OpArgK", "OpArgK", "iABC"), 
			opmode(1, 0, "OpArgK", "OpArgK", "iABC"), 
			opmode(1, 1, "OpArgR", "OpArgU", "iABC"),   
			opmode(1, 1, "OpArgR", "OpArgU", "iABC"),   
			opmode(0, 1, "OpArgU", "OpArgU", "iABC"),  
			opmode(0, 1, "OpArgU", "OpArgU", "iABC"),  
			opmode(0, 0, "OpArgU", "OpArgN", "iABC"),   
			opmode(0, 1, "OpArgR", "OpArgN", "iAsBx"),  
			opmode(0, 1, "OpArgR", "OpArgN", "iAsBx"),   
			opmode(1, 0, "OpArgN", "OpArgU", "iABC"),    
			opmode(0, 0, "OpArgU", "OpArgU", "iABC"),   
			opmode(0, 0, "OpArgN", "OpArgN", "iABC"),     
			opmode(0, 1, "OpArgU", "OpArgN", "iABx"),  
			opmode(0, 1, "OpArgU", "OpArgN", "iABC"),     
		}

		luaP.opmodes[0] =
			opmode(0, 1, "OpArgR", "OpArgN", "iABC")

		luaU.LUA_SIGNATURE = "\27Lua"

		luaU.LUA_TNUMBER  = 3
		luaU.LUA_TSTRING  = 4
		luaU.LUA_TNIL     = 0
		luaU.LUA_TBOOLEAN = 1
		luaU.LUA_TNONE    = -1

		luaU.LUAC_VERSION    = 0x51    
		luaU.LUAC_FORMAT     = 0     
		luaU.LUAC_HEADERSIZE = 12   

		function luaU:make_setS()
			local buff = {}
			buff.data = ""
			local writer =
				function(s, buff)
					if not s then return 0 end
					buff.data = buff.data..s
					return 0
				end
			return writer, buff
		end

		function luaU:make_setF(filename)
			local buff = {}
			buff.h = io.open(filename, "wb")
			if not buff.h then return nil end
			local writer =
				function(s, buff)  
					if not buff.h then return 0 end
					if not s then
					if buff.h:close() then return 0 end
				else
					if buff.h:write(s) then return 0 end
				end
					return 1
				end
			return writer, buff
		end

		function luaU:ttype(o)
			local tt = type(o.value)
			if tt == "number" then return self.LUA_TNUMBER
			elseif tt == "string" then return self.LUA_TSTRING
			elseif tt == "nil" then return self.LUA_TNIL
			elseif tt == "boolean" then return self.LUA_TBOOLEAN
			else
				return self.LUA_TNONE
			end
		end

		function luaU:from_double(x)
			local function grab_byte(v)
				local c = v % 256
				return (v - c) / 256, string.char(c)
			end
			local sign = 0
			if x < 0 then sign = 1; x = -x end
			local mantissa, exponent = math.frexp(x)
			if x == 0 then 
				mantissa, exponent = 0, 0
			elseif x == 1/0 then
				mantissa, exponent = 0, 2047
			else
				mantissa = (mantissa * 2 - 1) * math.ldexp(0.5, 53)
				exponent = exponent + 1022
			end
			local v, byte = ""
			x = math.floor(mantissa)
			for i = 1,6 do
				x, byte = grab_byte(x); v = v..byte 
			end
			x, byte = grab_byte(exponent * 16 + x); v = v..byte
			x, byte = grab_byte(sign * 128 + x); v = v..byte 
			return v
		end

		function luaU:from_int(x)
			local v = ""
			x = math.floor(x)
			if x < 0 then x = 4294967296 + x end  
			for i = 1, 4 do
				local c = x % 256
				v = v..string.char(c); x = math.floor(x / 256)
			end
			return v
		end

		function luaU:DumpBlock(b, D)
			if D.status == 0 then
				D.status = D.write(b, D.data)
			end
		end

		function luaU:DumpChar(y, D)
			self:DumpBlock(string.char(y), D)
		end

		function luaU:DumpInt(x, D)
			self:DumpBlock(self:from_int(x), D)
		end

		function luaU:DumpSizeT(x, D)
			self:DumpBlock(self:from_int(x), D)
			if size_size_t == 8 then
				self:DumpBlock(self:from_int(0), D)
			end
		end

		function luaU:DumpNumber(x, D)
			self:DumpBlock(self:from_double(x), D)
		end

		function luaU:DumpString(s, D)
			if s == nil then
				self:DumpSizeT(0, D)
			else
				s = s.."\0"
				self:DumpSizeT(#s, D)
				self:DumpBlock(s, D)
			end
		end

		function luaU:DumpCode(f, D)
			local n = f.sizecode
			self:DumpInt(n, D)
			for i = 0, n - 1 do
				self:DumpBlock(luaP:Instruction(f.code[i]), D)
			end
		end

		function luaU:DumpConstants(f, D)
			local n = f.sizek
			self:DumpInt(n, D)
			for i = 0, n - 1 do
				local o = f.k[i] 
				local tt = self:ttype(o)
				self:DumpChar(tt, D)
				if tt == self.LUA_TNIL then
				elseif tt == self.LUA_TBOOLEAN then
					self:DumpChar(o.value and 1 or 0, D)
				elseif tt == self.LUA_TNUMBER then
					self:DumpNumber(o.value, D)
				elseif tt == self.LUA_TSTRING then
					self:DumpString(o.value, D)
				else

				end
			end
			n = f.sizep
			self:DumpInt(n, D)
			for i = 0, n - 1 do
				self:DumpFunction(f.p[i], f.source, D)
			end
		end

		function luaU:DumpDebug(f, D)
			local n
			n = D.strip and 0 or f.sizelineinfo        
			--was DumpVector
			self:DumpInt(n, D)
			for i = 0, n - 1 do
				self:DumpInt(f.lineinfo[i], D)
			end
			n = D.strip and 0 or f.sizelocvars      
			self:DumpInt(n, D)
			for i = 0, n - 1 do
				self:DumpString(f.locvars[i].varname, D)
				self:DumpInt(f.locvars[i].startpc, D)
				self:DumpInt(f.locvars[i].endpc, D)
			end
			n = D.strip and 0 or f.sizeupvalues     
			self:DumpInt(n, D)
			for i = 0, n - 1 do
				self:DumpString(f.upvalues[i], D)
			end
		end

		function luaU:DumpFunction(f, p, D)
			local source = f.source
			if source == p or D.strip then source = nil end
			self:DumpString(source, D)
			self:DumpInt(f.lineDefined, D)
			self:DumpInt(f.lastlinedefined, D)
			self:DumpChar(f.nups, D)
			self:DumpChar(f.numparams, D)
			self:DumpChar(f.is_vararg, D)
			self:DumpChar(f.maxstacksize, D)
			self:DumpCode(f, D)
			self:DumpConstants(f, D)
			self:DumpDebug(f, D)
		end

		function luaU:DumpHeader(D)
			local h = self:header()
			assert(#h == self.LUAC_HEADERSIZE)
			self:DumpBlock(h, D)
		end

		function luaU:header()
			local x = 1
			return self.LUA_SIGNATURE..
				string.char(
					self.LUAC_VERSION,
					self.LUAC_FORMAT,
					x,                  
					4,                    
					size_size_t,                
					4,                  
					8,                  
					0)                  
		end

		function luaU:dump(L, f, w, data, strip)
			local D = {} 
			D.L = L
			D.write = w
			D.data = data
			D.strip = strip
			D.status = 0
			self:DumpHeader(D)
			self:DumpFunction(f, nil, D)
			D.write(nil, D.data)
			return D.status
		end
		luaK.MAXSTACK = 250

		function luaK:ttisnumber(o)
			if o then return type(o.value) == "number" else return false end
		end
		function luaK:nvalue(o) return o.value end
		function luaK:setnilvalue(o) o.value = nil end
		function luaK:setsvalue(o, x) o.value = x end
		luaK.setnvalue = luaK.setsvalue
		luaK.sethvalue = luaK.setsvalue
		luaK.setbvalue = luaK.setsvalue

		function luaK:numadd(a, b) return a + b end
		function luaK:numsub(a, b) return a - b end
		function luaK:nummul(a, b) return a * b end
		function luaK:numdiv(a, b) return a / b end
		function luaK:nummod(a, b) return a % b end
		function luaK:numpow(a, b) return a ^ b end
		function luaK:numunm(a) return -a end
		function luaK:numisnan(a) return not a == a end

		luaK.NO_JUMP = -1

		luaK.BinOpr = {
			OPR_ADD = 0, OPR_SUB = 1, OPR_MUL = 2, OPR_DIV = 3, OPR_MOD = 4, OPR_POW = 5,
			OPR_CONCAT = 6,
			OPR_NE = 7, OPR_EQ = 8,
			OPR_LT = 9, OPR_LE = 10, OPR_GT = 11, OPR_GE = 12,
			OPR_AND = 13, OPR_OR = 14,
			OPR_NOBINOPR = 15,
		}

		luaK.UnOpr = {
			OPR_MINUS = 0, OPR_NOT = 1, OPR_LEN = 2, OPR_NOUNOPR = 3
		}

		function luaK:getcode(fs, e)
			return fs.f.code[e.info]
		end

		function luaK:codeAsBx(fs, o, A, sBx)
			return self:codeABx(fs, o, A, sBx + luaP.MAXARG_sBx)
		end

		------------------------------------------------------------------------
		-- set the expdesc e instruction for multiple returns, was a macro
		------------------------------------------------------------------------
		function luaK:setmultret(fs, e)
			self:setreturns(fs, e, luaY.LUA_MULTRET)
		end

		------------------------------------------------------------------------
		-- there is a jump if patch lists are not identical, was a macro
		-- * used in luaK:exp2reg(), luaK:exp2anyreg(), luaK:exp2val()
		------------------------------------------------------------------------
		function luaK:hasjumps(e)
			return e.t ~= e.f
		end

		------------------------------------------------------------------------
		-- true if the expression is a constant number (for constant folding)
		-- * used in constfolding(), infix()
		------------------------------------------------------------------------
		function luaK:isnumeral(e)
			return e.k == "VKNUM" and e.t == self.NO_JUMP and e.f == self.NO_JUMP
		end

		------------------------------------------------------------------------
		-- codes loading of nil, optimization done if consecutive locations
		-- * used in luaK:discharge2reg(), (lparser) luaY:adjust_assign()
		------------------------------------------------------------------------
		function luaK:_nil(fs, from, n)
			if fs.pc > fs.lasttarget then  -- no jumps to current position?
				if fs.pc == 0 then  -- function start?
					if from >= fs.nactvar then
						return  -- positions are already clean
					end
				else
					local previous = fs.f.code[fs.pc - 1]
					if luaP:GET_OPCODE(previous) == "OP_LOADNIL" then
						local pfrom = luaP:GETARG_A(previous)
						local pto = luaP:GETARG_B(previous)
						if pfrom <= from and from <= pto + 1 then  -- can connect both?
							if from + n - 1 > pto then
								luaP:SETARG_B(previous, from + n - 1)
							end
							return
						end
					end
				end
			end
			self:codeABC(fs, "OP_LOADNIL", from, from + n - 1, 0)  -- else no optimization
		end

		------------------------------------------------------------------------
		--
		-- * used in multiple locations
		------------------------------------------------------------------------
		function luaK:jump(fs)
			local jpc = fs.jpc  -- save list of jumps to here
			fs.jpc = self.NO_JUMP
			local j = self:codeAsBx(fs, "OP_JMP", 0, self.NO_JUMP)
			j = self:concat(fs, j, jpc)  -- keep them on hold
			return j
		end

		------------------------------------------------------------------------
		-- codes a RETURN instruction
		-- * used in luaY:close_func(), luaY:retstat()
		------------------------------------------------------------------------
		function luaK:ret(fs, first, nret)
			self:codeABC(fs, "OP_RETURN", first, nret + 1, 0)
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:jumponcond(), luaK:codecomp()
		------------------------------------------------------------------------
		function luaK:condjump(fs, op, A, B, C)
			self:codeABC(fs, op, A, B, C)
			return self:jump(fs)
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:patchlistaux(), luaK:concat()
		------------------------------------------------------------------------
		function luaK:fixjump(fs, pc, dest)
			local jmp = fs.f.code[pc]
			local offset = dest - (pc + 1)
			lua_assert(dest ~= self.NO_JUMP)
			if math.abs(offset) > luaP.MAXARG_sBx then
				luaX:syntaxerror(fs.ls, "control structure too long")
			end
			luaP:SETARG_sBx(jmp, offset)
		end

		------------------------------------------------------------------------
		-- returns current 'pc' and marks it as a jump target (to avoid wrong
		-- optimizations with consecutive instructions not in the same basic block).
		-- * used in multiple locations
		-- * fs.lasttarget tested only by luaK:_nil() when optimizing OP_LOADNIL
		------------------------------------------------------------------------
		function luaK:getlabel(fs)
			fs.lasttarget = fs.pc
			return fs.pc
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:need_value(), luaK:removevalues(), luaK:patchlistaux(),
		--   luaK:concat()
		------------------------------------------------------------------------
		function luaK:getjump(fs, pc)
			local offset = luaP:GETARG_sBx(fs.f.code[pc])
			if offset == self.NO_JUMP then  -- point to itself represents end of list
				return self.NO_JUMP  -- end of list
			else
				return (pc + 1) + offset  -- turn offset into absolute position
			end
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:need_value(), luaK:patchtestreg(), luaK:invertjump()
		------------------------------------------------------------------------
		function luaK:getjumpcontrol(fs, pc)
			local pi = fs.f.code[pc]
			local ppi = fs.f.code[pc - 1]
			if pc >= 1 and luaP:testTMode(luaP:GET_OPCODE(ppi)) ~= 0 then
				return ppi
			else
				return pi
			end
		end

		------------------------------------------------------------------------
		-- check whether list has any jump that do not produce a value
		-- (or produce an inverted value)
		-- * return value changed to boolean
		-- * used only in luaK:exp2reg()
		------------------------------------------------------------------------
		function luaK:need_value(fs, list)
			while list ~= self.NO_JUMP do
				local i = self:getjumpcontrol(fs, list)
				if luaP:GET_OPCODE(i) ~= "OP_TESTSET" then return true end
				list = self:getjump(fs, list)
			end
			return false  -- not found
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:removevalues(), luaK:patchlistaux()
		------------------------------------------------------------------------
		function luaK:patchtestreg(fs, node, reg)
			local i = self:getjumpcontrol(fs, node)
			if luaP:GET_OPCODE(i) ~= "OP_TESTSET" then
				return false  -- cannot patch other instructions
			end
			if reg ~= luaP.NO_REG and reg ~= luaP:GETARG_B(i) then
				luaP:SETARG_A(i, reg)
			else  -- no register to put value or register already has the value
				-- due to use of a table as i, i cannot be replaced by another table
				-- so the following is required; there is no change to ARG_C
				luaP:SET_OPCODE(i, "OP_TEST")
				local b = luaP:GETARG_B(i)
				luaP:SETARG_A(i, b)
				luaP:SETARG_B(i, 0)
				-- *i = CREATE_ABC(OP_TEST, GETARG_B(*i), 0, GETARG_C(*i)); /* C */
			end
			return true
		end

		------------------------------------------------------------------------
		--
		-- * used only in luaK:codenot()
		------------------------------------------------------------------------
		function luaK:removevalues(fs, list)
			while list ~= self.NO_JUMP do
				self:patchtestreg(fs, list, luaP.NO_REG)
				list = self:getjump(fs, list)
			end
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:dischargejpc(), luaK:patchlist(), luaK:exp2reg()
		------------------------------------------------------------------------
		function luaK:patchlistaux(fs, list, vtarget, reg, dtarget)
			while list ~= self.NO_JUMP do
				local _next = self:getjump(fs, list)
				if self:patchtestreg(fs, list, reg) then
					self:fixjump(fs, list, vtarget)
				else
					self:fixjump(fs, list, dtarget)  -- jump to default target
				end
				list = _next
			end
		end

		------------------------------------------------------------------------
		--
		-- * used only in luaK:code()
		------------------------------------------------------------------------
		function luaK:dischargejpc(fs)
			self:patchlistaux(fs, fs.jpc, fs.pc, luaP.NO_REG, fs.pc)
			fs.jpc = self.NO_JUMP
		end

		------------------------------------------------------------------------
		--
		-- * used in (lparser) luaY:whilestat(), luaY:repeatstat(), luaY:forbody()
		------------------------------------------------------------------------
		function luaK:patchlist(fs, list, target)
			if target == fs.pc then
				self:patchtohere(fs, list)
			else
				lua_assert(target < fs.pc)
				self:patchlistaux(fs, list, target, luaP.NO_REG, target)
			end
		end

		------------------------------------------------------------------------
		--
		-- * used in multiple locations
		------------------------------------------------------------------------
		function luaK:patchtohere(fs, list)
			self:getlabel(fs)
			fs.jpc = self:concat(fs, fs.jpc, list)
		end

		------------------------------------------------------------------------
		-- * l1 was a pointer, now l1 is returned and callee assigns the value
		-- * used in multiple locations
		------------------------------------------------------------------------
		function luaK:concat(fs, l1, l2)
			if l2 == self.NO_JUMP then return l1
			elseif l1 == self.NO_JUMP then
				return l2
			else
				local list = l1
				local _next = self:getjump(fs, list)
				while _next ~= self.NO_JUMP do  -- find last element
					list = _next
					_next = self:getjump(fs, list)
				end
				self:fixjump(fs, list, l2)
			end
			return l1
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:reserveregs(), (lparser) luaY:forlist()
		------------------------------------------------------------------------
		function luaK:checkstack(fs, n)
			local newstack = fs.freereg + n
			if newstack > fs.f.maxstacksize then
				if newstack >= self.MAXSTACK then
					luaX:syntaxerror(fs.ls, "function or expression too complex")
				end
				fs.f.maxstacksize = newstack
			end
		end

		------------------------------------------------------------------------
		--
		-- * used in multiple locations
		------------------------------------------------------------------------
		function luaK:reserveregs(fs, n)
			self:checkstack(fs, n)
			fs.freereg = fs.freereg + n
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:freeexp(), luaK:dischargevars()
		------------------------------------------------------------------------
		function luaK:freereg(fs, reg)
			if not luaP:ISK(reg) and reg >= fs.nactvar then
				fs.freereg = fs.freereg - 1
				lua_assert(reg == fs.freereg)
			end
		end

		------------------------------------------------------------------------
		--
		-- * used in multiple locations
		------------------------------------------------------------------------
		function luaK:freeexp(fs, e)
			if e.k == "VNONRELOC" then
				self:freereg(fs, e.info)
			end
		end

		------------------------------------------------------------------------
		-- * TODO NOTE implementation is not 100% correct, since the assert fails
		-- * luaH_set, setobj deleted; direct table access used instead
		-- * used in luaK:stringK(), luaK:numberK(), luaK:boolK(), luaK:nilK()
		------------------------------------------------------------------------
		function luaK:addk(fs, k, v)
			local L = fs.L
			local idx = fs.h[k.value]
			--TValue *idx = luaH_set(L, fs->h, k); /* C */
			local f = fs.f
			if self:ttisnumber(idx) then
				--TODO this assert currently FAILS (last tested for 5.0.2)
				--lua_assert(fs.f.k[self:nvalue(idx)] == v)
				--lua_assert(luaO_rawequalObj(&fs->f->k[cast_int(nvalue(idx))], v)); /* C */
				return self:nvalue(idx)
			else -- constant not found; create a new entry
				idx = {}
				self:setnvalue(idx, fs.nk)
				fs.h[k.value] = idx
				-- setnvalue(idx, cast_num(fs->nk)); /* C */
				luaY:growvector(L, f.k, fs.nk, f.sizek, nil,
					luaP.MAXARG_Bx, "constant table overflow")
				-- loop to initialize empty f.k positions not required
				f.k[fs.nk] = v
				-- setobj(L, &f->k[fs->nk], v); /* C */
				-- luaC_barrier(L, f, v); /* GC */
				local nk = fs.nk
				fs.nk = fs.nk + 1
				return nk
			end

		end

		------------------------------------------------------------------------
		-- creates and sets a string object
		-- * used in (lparser) luaY:codestring(), luaY:singlevar()
		------------------------------------------------------------------------
		function luaK:stringK(fs, s)
			local o = {}  -- TValue
			self:setsvalue(o, s)
			return self:addk(fs, o, o)
		end

		------------------------------------------------------------------------
		-- creates and sets a number object
		-- * used in luaK:prefix() for negative (or negation of) numbers
		-- * used in (lparser) luaY:simpleexp(), luaY:fornum()
		------------------------------------------------------------------------
		function luaK:numberK(fs, r)
			local o = {}  -- TValue
			self:setnvalue(o, r)
			return self:addk(fs, o, o)
		end

		------------------------------------------------------------------------
		-- creates and sets a boolean object
		-- * used only in luaK:exp2RK()
		------------------------------------------------------------------------
		function luaK:boolK(fs, b)
			local o = {}  -- TValue
			self:setbvalue(o, b)
			return self:addk(fs, o, o)
		end

		------------------------------------------------------------------------
		-- creates and sets a nil object
		-- * used only in luaK:exp2RK()
		------------------------------------------------------------------------
		function luaK:nilK(fs)
			local k, v = {}, {}  -- TValue
			self:setnilvalue(v)
			-- cannot use nil as key; instead use table itself to represent nil
			self:sethvalue(k, fs.h)
			return self:addk(fs, k, v)
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:setmultret(), (lparser) luaY:adjust_assign()
		------------------------------------------------------------------------
		function luaK:setreturns(fs, e, nresults)
			if e.k == "VCALL" then  -- expression is an open function call?
				luaP:SETARG_C(self:getcode(fs, e), nresults + 1)
			elseif e.k == "VVARARG" then
				luaP:SETARG_B(self:getcode(fs, e), nresults + 1);
				luaP:SETARG_A(self:getcode(fs, e), fs.freereg);
				luaK:reserveregs(fs, 1)
			end
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:dischargevars(), (lparser) luaY:assignment()
		------------------------------------------------------------------------
		function luaK:setoneret(fs, e)
			if e.k == "VCALL" then  -- expression is an open function call?
				e.k = "VNONRELOC"
				e.info = luaP:GETARG_A(self:getcode(fs, e))
			elseif e.k == "VVARARG" then
				luaP:SETARG_B(self:getcode(fs, e), 2)
				e.k = "VRELOCABLE"  -- can relocate its simple result
			end
		end

		------------------------------------------------------------------------
		--
		-- * used in multiple locations
		------------------------------------------------------------------------
		function luaK:dischargevars(fs, e)
			local k = e.k
			if k == "VLOCAL" then
				e.k = "VNONRELOC"
			elseif k == "VUPVAL" then
				e.info = self:codeABC(fs, "OP_GETUPVAL", 0, e.info, 0)
				e.k = "VRELOCABLE"
			elseif k == "VGLOBAL" then
				e.info = self:codeABx(fs, "OP_GETGLOBAL", 0, e.info)
				e.k = "VRELOCABLE"
			elseif k == "VINDEXED" then
				self:freereg(fs, e.aux)
				self:freereg(fs, e.info)
				e.info = self:codeABC(fs, "OP_GETTABLE", 0, e.info, e.aux)
				e.k = "VRELOCABLE"
			elseif k == "VVARARG" or k == "VCALL" then
				self:setoneret(fs, e)
			else
				-- there is one value available (somewhere)
			end
		end

		------------------------------------------------------------------------
		--
		-- * used only in luaK:exp2reg()
		------------------------------------------------------------------------
		function luaK:code_label(fs, A, b, jump)
			self:getlabel(fs)  -- those instructions may be jump targets
			return self:codeABC(fs, "OP_LOADBOOL", A, b, jump)
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:discharge2anyreg(), luaK:exp2reg()
		------------------------------------------------------------------------
		function luaK:discharge2reg(fs, e, reg)
			self:dischargevars(fs, e)
			local k = e.k
			if k == "VNIL" then
				self:_nil(fs, reg, 1)
			elseif k == "VFALSE" or k == "VTRUE" then
				self:codeABC(fs, "OP_LOADBOOL", reg, (e.k == "VTRUE") and 1 or 0, 0)
			elseif k == "VK" then
				self:codeABx(fs, "OP_LOADK", reg, e.info)
			elseif k == "VKNUM" then
				self:codeABx(fs, "OP_LOADK", reg, self:numberK(fs, e.nval))
			elseif k == "VRELOCABLE" then
				local pc = self:getcode(fs, e)
				luaP:SETARG_A(pc, reg)
			elseif k == "VNONRELOC" then
				if reg ~= e.info then
					self:codeABC(fs, "OP_MOVE", reg, e.info, 0)
				end
			else
				lua_assert(e.k == "VVOID" or e.k == "VJMP")
				return  -- nothing to do...
			end
			e.info = reg
			e.k = "VNONRELOC"
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:jumponcond(), luaK:codenot()
		------------------------------------------------------------------------
		function luaK:discharge2anyreg(fs, e)
			if e.k ~= "VNONRELOC" then
				self:reserveregs(fs, 1)
				self:discharge2reg(fs, e, fs.freereg - 1)
			end
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:exp2nextreg(), luaK:exp2anyreg(), luaK:storevar()
		------------------------------------------------------------------------
		function luaK:exp2reg(fs, e, reg)
			self:discharge2reg(fs, e, reg)
			if e.k == "VJMP" then
				e.t = self:concat(fs, e.t, e.info)  -- put this jump in 't' list
			end
			if self:hasjumps(e) then
				local final  -- position after whole expression
				local p_f = self.NO_JUMP  -- position of an eventual LOAD false
				local p_t = self.NO_JUMP  -- position of an eventual LOAD true
				if self:need_value(fs, e.t) or self:need_value(fs, e.f) then
					local fj = (e.k == "VJMP") and self.NO_JUMP or self:jump(fs)
					p_f = self:code_label(fs, reg, 0, 1)
					p_t = self:code_label(fs, reg, 1, 0)
					self:patchtohere(fs, fj)
				end
				final = self:getlabel(fs)
				self:patchlistaux(fs, e.f, final, reg, p_f)
				self:patchlistaux(fs, e.t, final, reg, p_t)
			end
			e.f, e.t = self.NO_JUMP, self.NO_JUMP
			e.info = reg
			e.k = "VNONRELOC"
		end

		------------------------------------------------------------------------
		--
		-- * used in multiple locations
		------------------------------------------------------------------------
		function luaK:exp2nextreg(fs, e)
			self:dischargevars(fs, e)
			self:freeexp(fs, e)
			self:reserveregs(fs, 1)
			self:exp2reg(fs, e, fs.freereg - 1)
		end

		------------------------------------------------------------------------
		--
		-- * used in multiple locations
		------------------------------------------------------------------------
		function luaK:exp2anyreg(fs, e)
			self:dischargevars(fs, e)
			if e.k == "VNONRELOC" then
				if not self:hasjumps(e) then  -- exp is already in a register
					return e.info
				end
				if e.info >= fs.nactvar then  -- reg. is not a local?
					self:exp2reg(fs, e, e.info)  -- put value on it
					return e.info
				end
			end
			self:exp2nextreg(fs, e)  -- default
			return e.info
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:exp2RK(), luaK:prefix(), luaK:posfix()
		-- * used in (lparser) luaY:yindex()
		------------------------------------------------------------------------
		function luaK:exp2val(fs, e)
			if self:hasjumps(e) then
				self:exp2anyreg(fs, e)
			else
				self:dischargevars(fs, e)
			end
		end

		------------------------------------------------------------------------
		--
		-- * used in multiple locations
		------------------------------------------------------------------------
		function luaK:exp2RK(fs, e)
			self:exp2val(fs, e)
			local k = e.k
			if k == "VKNUM" or k == "VTRUE" or k == "VFALSE" or k == "VNIL" then
				if fs.nk <= luaP.MAXINDEXRK then  -- constant fit in RK operand?
					-- converted from a 2-deep ternary operator expression
					if e.k == "VNIL" then
						e.info = self:nilK(fs)
					else
						e.info = (e.k == "VKNUM") and self:numberK(fs, e.nval)
							or self:boolK(fs, e.k == "VTRUE")
					end
					e.k = "VK"
					return luaP:RKASK(e.info)
				end
			elseif k == "VK" then
				if e.info <= luaP.MAXINDEXRK then  -- constant fit in argC?
					return luaP:RKASK(e.info)
				end
			else
				-- default
			end
			-- not a constant in the right range: put it in a register
			return self:exp2anyreg(fs, e)
		end

		------------------------------------------------------------------------
		--
		-- * used in (lparser) luaY:assignment(), luaY:localfunc(), luaY:funcstat()
		------------------------------------------------------------------------
		function luaK:storevar(fs, var, ex)
			local k = var.k
			if k == "VLOCAL" then
				self:freeexp(fs, ex)
				self:exp2reg(fs, ex, var.info)
				return
			elseif k == "VUPVAL" then
				local e = self:exp2anyreg(fs, ex)
				self:codeABC(fs, "OP_SETUPVAL", e, var.info, 0)
			elseif k == "VGLOBAL" then
				local e = self:exp2anyreg(fs, ex)
				self:codeABx(fs, "OP_SETGLOBAL", e, var.info)
			elseif k == "VINDEXED" then
				local e = self:exp2RK(fs, ex)
				self:codeABC(fs, "OP_SETTABLE", var.info, var.aux, e)
			else
				lua_assert(0)  -- invalid var kind to store
			end
			self:freeexp(fs, ex)
		end

		------------------------------------------------------------------------
		--
		-- * used only in (lparser) luaY:primaryexp()
		------------------------------------------------------------------------
		function luaK:_self(fs, e, key)
			self:exp2anyreg(fs, e)
			self:freeexp(fs, e)
			local func = fs.freereg
			self:reserveregs(fs, 2)
			self:codeABC(fs, "OP_SELF", func, e.info, self:exp2RK(fs, key))
			self:freeexp(fs, key)
			e.info = func
			e.k = "VNONRELOC"
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:goiftrue(), luaK:codenot()
		------------------------------------------------------------------------
		function luaK:invertjump(fs, e)
			local pc = self:getjumpcontrol(fs, e.info)
			lua_assert(luaP:testTMode(luaP:GET_OPCODE(pc)) ~= 0 and
				luaP:GET_OPCODE(pc) ~= "OP_TESTSET" and
				luaP:GET_OPCODE(pc) ~= "OP_TEST")
			luaP:SETARG_A(pc, (luaP:GETARG_A(pc) == 0) and 1 or 0)
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:goiftrue(), luaK:goiffalse()
		------------------------------------------------------------------------
		function luaK:jumponcond(fs, e, cond)
			if e.k == "VRELOCABLE" then
				local ie = self:getcode(fs, e)
				if luaP:GET_OPCODE(ie) == "OP_NOT" then
					fs.pc = fs.pc - 1  -- remove previous OP_NOT
					return self:condjump(fs, "OP_TEST", luaP:GETARG_B(ie), 0, cond and 0 or 1)
				end
				-- else go through
			end
			self:discharge2anyreg(fs, e)
			self:freeexp(fs, e)
			return self:condjump(fs, "OP_TESTSET", luaP.NO_REG, e.info, cond and 1 or 0)
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:infix(), (lparser) luaY:cond()
		------------------------------------------------------------------------
		function luaK:goiftrue(fs, e)
			local pc  -- pc of last jump
			self:dischargevars(fs, e)
			local k = e.k
			if k == "VK" or k == "VKNUM" or k == "VTRUE" then
				pc = self.NO_JUMP  -- always true; do nothing
			elseif k == "VFALSE" then
				pc = self:jump(fs)  -- always jump
			elseif k == "VJMP" then
				self:invertjump(fs, e)
				pc = e.info
			else
				pc = self:jumponcond(fs, e, false)
			end
			e.f = self:concat(fs, e.f, pc)  -- insert last jump in `f' list
			self:patchtohere(fs, e.t)
			e.t = self.NO_JUMP
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:infix()
		------------------------------------------------------------------------
		function luaK:goiffalse(fs, e)
			local pc  -- pc of last jump
			self:dischargevars(fs, e)
			local k = e.k
			if k == "VNIL" or k == "VFALSE"then
				pc = self.NO_JUMP  -- always false; do nothing
			elseif k == "VTRUE" then
				pc = self:jump(fs)  -- always jump
			elseif k == "VJMP" then
				pc = e.info
			else
				pc = self:jumponcond(fs, e, true)
			end
			e.t = self:concat(fs, e.t, pc)  -- insert last jump in `t' list
			self:patchtohere(fs, e.f)
			e.f = self.NO_JUMP
		end

		------------------------------------------------------------------------
		--
		-- * used only in luaK:prefix()
		------------------------------------------------------------------------
		function luaK:codenot(fs, e)
			self:dischargevars(fs, e)
			local k = e.k
			if k == "VNIL" or k == "VFALSE" then
				e.k = "VTRUE"
			elseif k == "VK" or k == "VKNUM" or k == "VTRUE" then
				e.k = "VFALSE"
			elseif k == "VJMP" then
				self:invertjump(fs, e)
			elseif k == "VRELOCABLE" or k == "VNONRELOC" then
				self:discharge2anyreg(fs, e)
				self:freeexp(fs, e)
				e.info = self:codeABC(fs, "OP_NOT", 0, e.info, 0)
				e.k = "VRELOCABLE"
			else
				lua_assert(0)  -- cannot happen
			end
			-- interchange true and false lists
			e.f, e.t = e.t, e.f
			self:removevalues(fs, e.f)
			self:removevalues(fs, e.t)
		end

		------------------------------------------------------------------------
		--
		-- * used in (lparser) luaY:field(), luaY:primaryexp()
		------------------------------------------------------------------------
		function luaK:indexed(fs, t, k)
			t.aux = self:exp2RK(fs, k)
			t.k = "VINDEXED"
		end

		------------------------------------------------------------------------
		--
		-- * used only in luaK:codearith()
		------------------------------------------------------------------------
		function luaK:constfolding(op, e1, e2)
			local r
			if not self:isnumeral(e1) or not self:isnumeral(e2) then return false end
			local v1 = e1.nval
			local v2 = e2.nval
			if op == "OP_ADD" then
				r = self:numadd(v1, v2)
			elseif op == "OP_SUB" then
				r = self:numsub(v1, v2)
			elseif op == "OP_MUL" then
				r = self:nummul(v1, v2)
			elseif op == "OP_DIV" then
				if v2 == 0 then return false end  -- do not attempt to divide by 0
				r = self:numdiv(v1, v2)
			elseif op == "OP_MOD" then
				if v2 == 0 then return false end  -- do not attempt to divide by 0
				r = self:nummod(v1, v2)
			elseif op == "OP_POW" then
				r = self:numpow(v1, v2)
			elseif op == "OP_UNM" then
				r = self:numunm(v1)
			elseif op == "OP_LEN" then
				return false  -- no constant folding for 'len'
			else
				lua_assert(0)
				r = 0
			end
			if self:numisnan(r) then return false end  -- do not attempt to produce NaN
			e1.nval = r
			return true
		end

		------------------------------------------------------------------------
		--
		-- * used in luaK:prefix(), luaK:posfix()
		------------------------------------------------------------------------
		function luaK:codearith(fs, op, e1, e2)
			if self:constfolding(op, e1, e2) then
				return
			else
				local o2 = (op ~= "OP_UNM" and op ~= "OP_LEN") and self:exp2RK(fs, e2) or 0
				local o1 = self:exp2RK(fs, e1)
				if o1 > o2 then
					self:freeexp(fs, e1)
					self:freeexp(fs, e2)
				else
					self:freeexp(fs, e2)
					self:freeexp(fs, e1)
				end
				e1.info = self:codeABC(fs, op, 0, o1, o2)
				e1.k = "VRELOCABLE"
			end
		end

		------------------------------------------------------------------------
		--
		-- * used only in luaK:posfix()
		------------------------------------------------------------------------
		function luaK:codecomp(fs, op, cond, e1, e2)
			local o1 = self:exp2RK(fs, e1)
			local o2 = self:exp2RK(fs, e2)
			self:freeexp(fs, e2)
			self:freeexp(fs, e1)
			if cond == 0 and op ~= "OP_EQ" then
				-- exchange args to replace by `<' or `<='
				o1, o2 = o2, o1  -- o1 <==> o2
				cond = 1
			end
			e1.info = self:condjump(fs, op, cond, o1, o2)
			e1.k = "VJMP"
		end

		------------------------------------------------------------------------
		--
		-- * used only in (lparser) luaY:subexpr()
		------------------------------------------------------------------------
		function luaK:prefix(fs, op, e)
			local e2 = {}  -- expdesc
			e2.t, e2.f = self.NO_JUMP, self.NO_JUMP
			e2.k = "VKNUM"
			e2.nval = 0
			if op == "OPR_MINUS" then
				if not self:isnumeral(e) then
					self:exp2anyreg(fs, e)  -- cannot operate on non-numeric constants
				end
				self:codearith(fs, "OP_UNM", e, e2)
			elseif op == "OPR_NOT" then
				self:codenot(fs, e)
			elseif op == "OPR_LEN" then
				self:exp2anyreg(fs, e)  -- cannot operate on constants
				self:codearith(fs, "OP_LEN", e, e2)
			else
				lua_assert(0)
			end
		end

		------------------------------------------------------------------------
		--
		-- * used only in (lparser) luaY:subexpr()
		------------------------------------------------------------------------
		function luaK:infix(fs, op, v)
			if op == "OPR_AND" then
				self:goiftrue(fs, v)
			elseif op == "OPR_OR" then
				self:goiffalse(fs, v)
			elseif op == "OPR_CONCAT" then
				self:exp2nextreg(fs, v)  -- operand must be on the 'stack'
			elseif op == "OPR_ADD" or op == "OPR_SUB" or
				op == "OPR_MUL" or op == "OPR_DIV" or
				op == "OPR_MOD" or op == "OPR_POW" then
				if not self:isnumeral(v) then self:exp2RK(fs, v) end
			else
				self:exp2RK(fs, v)
			end
		end

		------------------------------------------------------------------------
		--
		-- * used only in (lparser) luaY:subexpr()
		------------------------------------------------------------------------
		-- table lookups to simplify testing
		luaK.arith_op = {
			OPR_ADD = "OP_ADD", OPR_SUB = "OP_SUB", OPR_MUL = "OP_MUL",
			OPR_DIV = "OP_DIV", OPR_MOD = "OP_MOD", OPR_POW = "OP_POW",
		}
		luaK.comp_op = {
			OPR_EQ = "OP_EQ", OPR_NE = "OP_EQ", OPR_LT = "OP_LT",
			OPR_LE = "OP_LE", OPR_GT = "OP_LT", OPR_GE = "OP_LE",
		}
		luaK.comp_cond = {
			OPR_EQ = 1, OPR_NE = 0, OPR_LT = 1,
			OPR_LE = 1, OPR_GT = 0, OPR_GE = 0,
		}
		function luaK:posfix(fs, op, e1, e2)
			-- needed because e1 = e2 doesn't copy values...
			-- * in 5.0.x, only k/info/aux/t/f copied, t for AND, f for OR
			--   but here, all elements are copied for completeness' sake
			local function copyexp(e1, e2)
				e1.k = e2.k
				e1.info = e2.info; e1.aux = e2.aux
				e1.nval = e2.nval
				e1.t = e2.t; e1.f = e2.f
			end
			if op == "OPR_AND" then
				lua_assert(e1.t == self.NO_JUMP)  -- list must be closed
				self:dischargevars(fs, e2)
				e2.f = self:concat(fs, e2.f, e1.f)
				copyexp(e1, e2)
			elseif op == "OPR_OR" then
				lua_assert(e1.f == self.NO_JUMP)  -- list must be closed
				self:dischargevars(fs, e2)
				e2.t = self:concat(fs, e2.t, e1.t)
				copyexp(e1, e2)
			elseif op == "OPR_CONCAT" then
				self:exp2val(fs, e2)
				if e2.k == "VRELOCABLE" and luaP:GET_OPCODE(self:getcode(fs, e2)) == "OP_CONCAT" then
					lua_assert(e1.info == luaP:GETARG_B(self:getcode(fs, e2)) - 1)
					self:freeexp(fs, e1)
					luaP:SETARG_B(self:getcode(fs, e2), e1.info)
					e1.k = "VRELOCABLE"
					e1.info = e2.info
				else
					self:exp2nextreg(fs, e2)  -- operand must be on the 'stack'
					self:codearith(fs, "OP_CONCAT", e1, e2)
				end
			else
				-- the following uses a table lookup in place of conditionals
				local arith = self.arith_op[op]
				if arith then
					self:codearith(fs, arith, e1, e2)
				else
					local comp = self.comp_op[op]
					if comp then
						self:codecomp(fs, comp, self.comp_cond[op], e1, e2)
					else
						lua_assert(0)
					end
				end--if arith
			end--if op
		end

		------------------------------------------------------------------------
		-- adjusts debug information for last instruction written, in order to
		-- change the line where item comes into existence
		-- * used in (lparser) luaY:funcargs(), luaY:forbody(), luaY:funcstat()
		------------------------------------------------------------------------
		function luaK:fixline(fs, line)
			fs.f.lineinfo[fs.pc - 1] = line
		end

		------------------------------------------------------------------------
		-- general function to write an instruction into the instruction buffer,
		-- sets debug information too
		-- * used in luaK:codeABC(), luaK:codeABx()
		-- * called directly by (lparser) luaY:whilestat()
		------------------------------------------------------------------------
		function luaK:code(fs, i, line)
			local f = fs.f
			self:dischargejpc(fs)  -- 'pc' will change
			-- put new instruction in code array
			luaY:growvector(fs.L, f.code, fs.pc, f.sizecode, nil,
				luaY.MAX_INT, "code size overflow")
			f.code[fs.pc] = i
			-- save corresponding line information
			luaY:growvector(fs.L, f.lineinfo, fs.pc, f.sizelineinfo, nil,
				luaY.MAX_INT, "code size overflow")
			f.lineinfo[fs.pc] = line
			local pc = fs.pc
			fs.pc = fs.pc + 1
			return pc
		end

		------------------------------------------------------------------------
		-- writes an instruction of type ABC
		-- * calls luaK:code()
		------------------------------------------------------------------------
		function luaK:codeABC(fs, o, a, b, c)
			lua_assert(luaP:getOpMode(o) == luaP.OpMode.iABC)
			lua_assert(luaP:getBMode(o) ~= luaP.OpArgMask.OpArgN or b == 0)
			lua_assert(luaP:getCMode(o) ~= luaP.OpArgMask.OpArgN or c == 0)
			return self:code(fs, luaP:CREATE_ABC(o, a, b, c), fs.ls.lastline)
		end

		------------------------------------------------------------------------
		-- writes an instruction of type ABx
		-- * calls luaK:code(), called by luaK:codeAsBx()
		------------------------------------------------------------------------
		function luaK:codeABx(fs, o, a, bc)
			lua_assert(luaP:getOpMode(o) == luaP.OpMode.iABx or
				luaP:getOpMode(o) == luaP.OpMode.iAsBx)
			lua_assert(luaP:getCMode(o) == luaP.OpArgMask.OpArgN)
			return self:code(fs, luaP:CREATE_ABx(o, a, bc), fs.ls.lastline)
		end

		------------------------------------------------------------------------
		--
		-- * used in (lparser) luaY:closelistfield(), luaY:lastlistfield()
		------------------------------------------------------------------------
		function luaK:setlist(fs, base, nelems, tostore)
			local c = math.floor((nelems - 1)/luaP.LFIELDS_PER_FLUSH) + 1
			local b = (tostore == luaY.LUA_MULTRET) and 0 or tostore
			lua_assert(tostore ~= 0)
			if c <= luaP.MAXARG_C then
				self:codeABC(fs, "OP_SETLIST", base, b, c)
			else
				self:codeABC(fs, "OP_SETLIST", base, b, 0)
				self:code(fs, luaP:CREATE_Inst(c), fs.ls.lastline)
			end
			fs.freereg = base + 1  -- free registers with list values
		end




		--dofile("lparser.lua")

--[[--------------------------------------------------------------------
-- Expression descriptor
-- * expkind changed to string constants; luaY:assignment was the only
--   function to use a relational operator with this enumeration
-- VVOID       -- no value
-- VNIL        -- no value
-- VTRUE       -- no value
-- VFALSE      -- no value
-- VK          -- info = index of constant in 'k'
-- VKNUM       -- nval = numerical value
-- VLOCAL      -- info = local register
-- VUPVAL,     -- info = index of upvalue in 'upvalues'
-- VGLOBAL     -- info = index of table; aux = index of global name in 'k'
-- VINDEXED    -- info = table register; aux = index register (or 'k')
-- VJMP        -- info = instruction pc
-- VRELOCABLE  -- info = instruction pc
-- VNONRELOC   -- info = result register
-- VCALL       -- info = instruction pc
-- VVARARG     -- info = instruction pc
} ----------------------------------------------------------------------]]

--[[--------------------------------------------------------------------
-- * expdesc in Lua 5.1.x has a union u and another struct s; this Lua
--   implementation ignores all instances of u and s usage
-- struct expdesc:
--   k  -- (enum: expkind)
--   info, aux -- (int, int)
--   nval -- (lua_Number)
--   t  -- patch list of 'exit when true'
--   f  -- patch list of 'exit when false'
----------------------------------------------------------------------]]

--[[--------------------------------------------------------------------
-- struct upvaldesc:
--   k  -- (lu_byte)
--   info -- (lu_byte)
----------------------------------------------------------------------]]

--[[--------------------------------------------------------------------
-- state needed to generate code for a given function
-- struct FuncState:
--   f  -- current function header (table: Proto)
--   h  -- table to find (and reuse) elements in 'k' (table: Table)
--   prev  -- enclosing function (table: FuncState)
--   ls  -- lexical state (table: LexState)
--   L  -- copy of the Lua state (table: lua_State)
--   bl  -- chain of current blocks (table: BlockCnt)
--   pc  -- next position to code (equivalent to 'ncode')
--   lasttarget   -- 'pc' of last 'jump target'
--   jpc  -- list of pending jumps to 'pc'
--   freereg  -- first free register
--   nk  -- number of elements in 'k'
--   np  -- number of elements in 'p'
--   nlocvars  -- number of elements in 'locvars'
--   nactvar  -- number of active local variables
--   upvalues[LUAI_MAXUPVALUES]  -- upvalues (table: upvaldesc)
--   actvar[LUAI_MAXVARS]  -- declared-variable stack
----------------------------------------------------------------------]]

		------------------------------------------------------------------------
		-- constants used by parser
		-- * picks up duplicate values from luaX if required
		------------------------------------------------------------------------
		luaY.LUA_QS = luaX.LUA_QS or "'%s'"  -- (from luaconf.h)

		luaY.SHRT_MAX = 32767 -- (from <limits.h>)
		luaY.LUAI_MAXVARS = 200  -- (luaconf.h)
		luaY.LUAI_MAXUPVALUES = 60  -- (luaconf.h)
		luaY.MAX_INT = luaX.MAX_INT or 2147483645  -- (from llimits.h)
		-- * INT_MAX-2 for 32-bit systems
		luaY.LUAI_MAXCCALLS = 200  -- (from luaconf.h)

		luaY.VARARG_HASARG = 1  -- (from lobject.h)
		-- NOTE: HASARG_MASK is value-specific
		luaY.HASARG_MASK = 2 -- this was added for a bitop in parlist()
		luaY.VARARG_ISVARARG = 2
		-- NOTE: there is some value-specific code that involves VARARG_NEEDSARG
		luaY.VARARG_NEEDSARG = 4

		luaY.LUA_MULTRET = -1  -- (lua.h)

--[[--------------------------------------------------------------------
-- other functions
----------------------------------------------------------------------]]

		------------------------------------------------------------------------
		-- LUA_QL describes how error messages quote program elements.
		-- CHANGE it if you want a different appearance. (from luaconf.h)
		------------------------------------------------------------------------
		function luaY:LUA_QL(x)
			return "'"..x.."'"
		end

		------------------------------------------------------------------------
		-- this is a stripped-down luaM_growvector (from lmem.h) which is a
		-- macro based on luaM_growaux (in lmem.c); all the following does is
		-- reproduce the size limit checking logic of the original function
		-- so that error behaviour is identical; all arguments preserved for
		-- convenience, even those which are unused
		-- * set the t field to nil, since this originally does a sizeof(t)
		-- * size (originally a pointer) is never updated, their final values
		--   are set by luaY:close_func(), so overall things should still work
		------------------------------------------------------------------------
		function luaY:growvector(L, v, nelems, size, t, limit, e)
			if nelems >= limit then
				error(e)  -- was luaG_runerror
			end
		end

		------------------------------------------------------------------------
		-- initialize a new function prototype structure (from lfunc.c)
		-- * used only in open_func()
		------------------------------------------------------------------------
		function luaY:newproto(L)
			local f = {} -- Proto
			-- luaC_link(L, obj2gco(f), LUA_TPROTO); /* GC */
			f.k = {}
			f.sizek = 0
			f.p = {}
			f.sizep = 0
			f.code = {}
			f.sizecode = 0
			f.sizelineinfo = 0
			f.sizeupvalues = 0
			f.nups = 0
			f.upvalues = {}
			f.numparams = 0
			f.is_vararg = 0
			f.maxstacksize = 0
			f.lineinfo = {}
			f.sizelocvars = 0
			f.locvars = {}
			f.lineDefined = 0
			f.lastlinedefined = 0
			f.source = nil
			return f
		end

		------------------------------------------------------------------------
		-- converts an integer to a "floating point byte", represented as
		-- (eeeeexxx), where the real value is (1xxx) * 2^(eeeee - 1) if
		-- eeeee != 0 and (xxx) otherwise.
		------------------------------------------------------------------------
		function luaY:int2fb(x)
			local e = 0  -- exponent
			while x >= 16 do
				x = math.floor((x + 1) / 2)
				e = e + 1
			end
			if x < 8 then
				return x
			else
				return ((e + 1) * 8) + (x - 8)
			end
		end

--[[--------------------------------------------------------------------
-- parser functions
----------------------------------------------------------------------]]

		------------------------------------------------------------------------
		-- true of the kind of expression produces multiple return values
		------------------------------------------------------------------------
		function luaY:hasmultret(k)
			return k == "VCALL" or k == "VVARARG"
		end

		------------------------------------------------------------------------
		-- convenience function to access active local i, returns entry
		------------------------------------------------------------------------
		function luaY:getlocvar(fs, i)
			return fs.f.locvars[ fs.actvar[i] ]
		end

		------------------------------------------------------------------------
		-- check a limit, string m provided as an error message
		------------------------------------------------------------------------
		function luaY:checklimit(fs, v, l, m)
			if v > l then self:errorlimit(fs, l, m) end
		end

--[[--------------------------------------------------------------------
-- nodes for block list (list of active blocks)
-- struct BlockCnt:
--   previous  -- chain (table: BlockCnt)
--   breaklist  -- list of jumps out of this loop
--   nactvar  -- # active local variables outside the breakable structure
--   upval  -- true if some variable in the block is an upvalue (boolean)
--   isbreakable  -- true if 'block' is a loop (boolean)
----------------------------------------------------------------------]]

		------------------------------------------------------------------------
		-- prototypes for recursive non-terminal functions
		------------------------------------------------------------------------
		-- prototypes deleted; not required in Lua

		------------------------------------------------------------------------
		-- reanchor if last token is has a constant string, see close_func()
		-- * used only in close_func()
		------------------------------------------------------------------------
		function luaY:anchor_token(ls)
			if ls.t.token == "TK_NAME" or ls.t.token == "TK_STRING" then
				-- not relevant to Lua implementation of parser
				-- local ts = ls.t.seminfo
				-- luaX_newstring(ls, getstr(ts), ts->tsv.len); /* C */
			end
		end

		------------------------------------------------------------------------
		-- throws a syntax error if token expected is not there
		------------------------------------------------------------------------
		function luaY:error_expected(ls, token)
			luaX:syntaxerror(ls,
				string.format(self.LUA_QS.." expected", luaX:token2str(ls, token)))
		end

		------------------------------------------------------------------------
		-- prepares error message for display, for limits exceeded
		-- * used only in checklimit()
		------------------------------------------------------------------------
		function luaY:errorlimit(fs, limit, what)
			local msg = (fs.f.linedefined == 0) and
				string.format("main function has more than %d %s", limit, what) or
				string.format("function at line %d has more than %d %s",
					fs.f.linedefined, limit, what)
			luaX:lexerror(fs.ls, msg, 0)
		end

		------------------------------------------------------------------------
		-- tests for a token, returns outcome
		-- * return value changed to boolean
		------------------------------------------------------------------------
		function luaY:testnext(ls, c)
			if ls.t.token == c then
				luaX:next(ls)
				return true
			else
				return false
			end
		end

		------------------------------------------------------------------------
		-- check for existence of a token, throws error if not found
		------------------------------------------------------------------------
		function luaY:check(ls, c)
			if ls.t.token ~= c then
				self:error_expected(ls, c)
			end
		end

		------------------------------------------------------------------------
		-- verify existence of a token, then skip it
		------------------------------------------------------------------------
		function luaY:checknext(ls, c)
			self:check(ls, c)
			luaX:next(ls)
		end

		------------------------------------------------------------------------
		-- throws error if condition not matched
		------------------------------------------------------------------------
		function luaY:check_condition(ls, c, msg)
			if not c then luaX:syntaxerror(ls, msg) end
		end

		------------------------------------------------------------------------
		-- verifies token conditions are met or else throw error
		------------------------------------------------------------------------
		function luaY:check_match(ls, what, who, where)
			if not self:testnext(ls, what) then
				if where == ls.linenumber then
					self:error_expected(ls, what)
				else
					luaX:syntaxerror(ls, string.format(
						self.LUA_QS.." expected (to close "..self.LUA_QS.." at line %d)",
						luaX:token2str(ls, what), luaX:token2str(ls, who), where))
				end
			end
		end

		------------------------------------------------------------------------
		-- expect that token is a name, return the name
		------------------------------------------------------------------------
		function luaY:str_checkname(ls)
			self:check(ls, "TK_NAME")
			local ts = ls.t.seminfo
			luaX:next(ls)
			return ts
		end

		------------------------------------------------------------------------
		-- initialize a struct expdesc, expression description data structure
		------------------------------------------------------------------------
		function luaY:init_exp(e, k, i)
			e.f, e.t = luaK.NO_JUMP, luaK.NO_JUMP
			e.k = k
			e.info = i
		end

		------------------------------------------------------------------------
		-- adds given string s in string pool, sets e as VK
		------------------------------------------------------------------------
		function luaY:codestring(ls, e, s)
			self:init_exp(e, "VK", luaK:stringK(ls.fs, s))
		end

		------------------------------------------------------------------------
		-- consume a name token, adds it to string pool, sets e as VK
		------------------------------------------------------------------------
		function luaY:checkname(ls, e)
			self:codestring(ls, e, self:str_checkname(ls))
		end

		------------------------------------------------------------------------
		-- creates struct entry for a local variable
		-- * used only in new_localvar()
		------------------------------------------------------------------------
		function luaY:registerlocalvar(ls, varname)
			local fs = ls.fs
			local f = fs.f
			self:growvector(ls.L, f.locvars, fs.nlocvars, f.sizelocvars,
				nil, self.SHRT_MAX, "too many local variables")
			-- loop to initialize empty f.locvar positions not required
			f.locvars[fs.nlocvars] = {} -- LocVar
			f.locvars[fs.nlocvars].varname = varname
			-- luaC_objbarrier(ls.L, f, varname) /* GC */
			local nlocvars = fs.nlocvars
			fs.nlocvars = fs.nlocvars + 1
			return nlocvars
		end

		------------------------------------------------------------------------
		-- creates a new local variable given a name and an offset from nactvar
		-- * used in fornum(), forlist(), parlist(), body()
		------------------------------------------------------------------------
		function luaY:new_localvarliteral(ls, v, n)
			self:new_localvar(ls, v, n)
		end

		------------------------------------------------------------------------
		-- register a local variable, set in active variable list
		------------------------------------------------------------------------
		function luaY:new_localvar(ls, name, n)
			local fs = ls.fs
			self:checklimit(fs, fs.nactvar + n + 1, self.LUAI_MAXVARS, "local variables")
			fs.actvar[fs.nactvar + n] = self:registerlocalvar(ls, name)
		end

		------------------------------------------------------------------------
		-- adds nvars number of new local variables, set debug information
		------------------------------------------------------------------------
		function luaY:adjustlocalvars(ls, nvars)
			local fs = ls.fs
			fs.nactvar = fs.nactvar + nvars
			for i = nvars, 1, -1 do
				self:getlocvar(fs, fs.nactvar - i).startpc = fs.pc
			end
		end

		------------------------------------------------------------------------
		-- removes a number of locals, set debug information
		------------------------------------------------------------------------
		function luaY:removevars(ls, tolevel)
			local fs = ls.fs
			while fs.nactvar > tolevel do
				fs.nactvar = fs.nactvar - 1
				self:getlocvar(fs, fs.nactvar).endpc = fs.pc
			end
		end

		------------------------------------------------------------------------
		-- returns an existing upvalue index based on the given name, or
		-- creates a new upvalue struct entry and returns the new index
		-- * used only in singlevaraux()
		------------------------------------------------------------------------
		function luaY:indexupvalue(fs, name, v)
			local f = fs.f
			for i = 0, f.nups - 1 do
				if fs.upvalues[i].k == v.k and fs.upvalues[i].info == v.info then
					lua_assert(f.upvalues[i] == name)
					return i
				end
			end
			-- new one
			self:checklimit(fs, f.nups + 1, self.LUAI_MAXUPVALUES, "upvalues")
			self:growvector(fs.L, f.upvalues, f.nups, f.sizeupvalues,
				nil, self.MAX_INT, "")
			-- loop to initialize empty f.upvalues positions not required
			f.upvalues[f.nups] = name
			-- luaC_objbarrier(fs->L, f, name); /* GC */
			lua_assert(v.k == "VLOCAL" or v.k == "VUPVAL")
			-- this is a partial copy; only k & info fields used
			fs.upvalues[f.nups] = { k = v.k, info = v.info }
			local nups = f.nups
			f.nups = f.nups + 1
			return nups
		end

		------------------------------------------------------------------------
		-- search the local variable namespace of the given fs for a match
		-- * used only in singlevaraux()
		------------------------------------------------------------------------
		function luaY:searchvar(fs, n)
			for i = fs.nactvar - 1, 0, -1 do
				if n == self:getlocvar(fs, i).varname then
					return i
				end
			end
			return -1  -- not found
		end

		------------------------------------------------------------------------
		-- * mark upvalue flags in function states up to a given level
		-- * used only in singlevaraux()
		------------------------------------------------------------------------
		function luaY:markupval(fs, level)
			local bl = fs.bl
			while bl and bl.nactvar > level do bl = bl.previous end
			if bl then bl.upval = true end
		end

		------------------------------------------------------------------------
		-- handle locals, globals and upvalues and related processing
		-- * search mechanism is recursive, calls itself to search parents
		-- * used only in singlevar()
		------------------------------------------------------------------------
		function luaY:singlevaraux(fs, n, var, base)
			if fs == nil then  -- no more levels?
				self:init_exp(var, "VGLOBAL", luaP.NO_REG)  -- default is global variable
				return "VGLOBAL"
			else
				local v = self:searchvar(fs, n)  -- look up at current level
				if v >= 0 then
					self:init_exp(var, "VLOCAL", v)
					if base == 0 then
						self:markupval(fs, v)  -- local will be used as an upval
					end
					return "VLOCAL"
				else  -- not found at current level; try upper one
					if self:singlevaraux(fs.prev, n, var, 0) == "VGLOBAL" then
						return "VGLOBAL"
					end
					var.info = self:indexupvalue(fs, n, var)  -- else was LOCAL or UPVAL
					var.k = "VUPVAL"  -- upvalue in this level
					return "VUPVAL"
				end--if v
			end--if fs
		end

		------------------------------------------------------------------------
		-- consume a name token, creates a variable (global|local|upvalue)
		-- * used in prefixexp(), funcname()
		------------------------------------------------------------------------
		function luaY:singlevar(ls, var)
			local varname = self:str_checkname(ls)
			local fs = ls.fs
			if self:singlevaraux(fs, varname, var, 1) == "VGLOBAL" then
				var.info = luaK:stringK(fs, varname)  -- info points to global name
			end
		end

		------------------------------------------------------------------------
		-- adjust RHS to match LHS in an assignment
		-- * used in assignment(), forlist(), localstat()
		------------------------------------------------------------------------
		function luaY:adjust_assign(ls, nvars, nexps, e)
			local fs = ls.fs
			local extra = nvars - nexps
			if self:hasmultret(e.k) then
				extra = extra + 1  -- includes call itself
				if extra <= 0 then extra = 0 end
				luaK:setreturns(fs, e, extra)  -- last exp. provides the difference
				if extra > 1 then luaK:reserveregs(fs, extra - 1) end
			else
				if e.k ~= "VVOID" then luaK:exp2nextreg(fs, e) end  -- close last expression
				if extra > 0 then
					local reg = fs.freereg
					luaK:reserveregs(fs, extra)
					luaK:_nil(fs, reg, extra)
				end
			end
		end

		------------------------------------------------------------------------
		-- tracks and limits parsing depth, assert check at end of parsing
		------------------------------------------------------------------------
		function luaY:enterlevel(ls)
			ls.L.nCcalls = ls.L.nCcalls + 1
			if ls.L.nCcalls > self.LUAI_MAXCCALLS then
				luaX:lexerror(ls, "chunk has too many syntax levels", 0)
			end
		end

		------------------------------------------------------------------------
		-- tracks parsing depth, a pair with luaY:enterlevel()
		------------------------------------------------------------------------
		function luaY:leavelevel(ls)
			ls.L.nCcalls = ls.L.nCcalls - 1
		end

		------------------------------------------------------------------------
		-- enters a code unit, initializes elements
		------------------------------------------------------------------------
		function luaY:enterblock(fs, bl, isbreakable)
			bl.breaklist = luaK.NO_JUMP
			bl.isbreakable = isbreakable
			bl.nactvar = fs.nactvar
			bl.upval = false
			bl.previous = fs.bl
			fs.bl = bl
			lua_assert(fs.freereg == fs.nactvar)
		end

		------------------------------------------------------------------------
		-- leaves a code unit, close any upvalues
		------------------------------------------------------------------------
		function luaY:leaveblock(fs)
			local bl = fs.bl
			fs.bl = bl.previous
			self:removevars(fs.ls, bl.nactvar)
			if bl.upval then
				luaK:codeABC(fs, "OP_CLOSE", bl.nactvar, 0, 0)
			end
			-- a block either controls scope or breaks (never both)
			lua_assert(not bl.isbreakable or not bl.upval)
			lua_assert(bl.nactvar == fs.nactvar)
			fs.freereg = fs.nactvar  -- free registers
			luaK:patchtohere(fs, bl.breaklist)
		end

		------------------------------------------------------------------------
		-- implement the instantiation of a function prototype, append list of
		-- upvalues after the instantiation instruction
		-- * used only in body()
		------------------------------------------------------------------------
		function luaY:pushclosure(ls, func, v)
			local fs = ls.fs
			local f = fs.f
			self:growvector(ls.L, f.p, fs.np, f.sizep, nil,
				luaP.MAXARG_Bx, "constant table overflow")
			-- loop to initialize empty f.p positions not required
			f.p[fs.np] = func.f
			fs.np = fs.np + 1
			-- luaC_objbarrier(ls->L, f, func->f); /* C */
			self:init_exp(v, "VRELOCABLE", luaK:codeABx(fs, "OP_CLOSURE", 0, fs.np - 1))
			for i = 0, func.f.nups - 1 do
				local o = (func.upvalues[i].k == "VLOCAL") and "OP_MOVE" or "OP_GETUPVAL"
				luaK:codeABC(fs, o, 0, func.upvalues[i].info, 0)
			end
		end

		------------------------------------------------------------------------
		-- opening of a function
		------------------------------------------------------------------------
		function luaY:open_func(ls, fs)
			local L = ls.L
			local f = self:newproto(ls.L)
			fs.f = f
			fs.prev = ls.fs  -- linked list of funcstates
			fs.ls = ls
			fs.L = L
			ls.fs = fs
			fs.pc = 0
			fs.lasttarget = -1
			fs.jpc = luaK.NO_JUMP
			fs.freereg = 0
			fs.nk = 0
			fs.np = 0
			fs.nlocvars = 0
			fs.nactvar = 0
			fs.bl = nil
			f.source = ls.source
			f.maxstacksize = 2  -- registers 0/1 are always valid
			fs.h = {}  -- constant table; was luaH_new call
			-- anchor table of constants and prototype (to avoid being collected)
			-- sethvalue2s(L, L->top, fs->h); incr_top(L); /* C */
			-- setptvalue2s(L, L->top, f); incr_top(L);
		end

		------------------------------------------------------------------------
		-- closing of a function
		------------------------------------------------------------------------
		function luaY:close_func(ls)
			local L = ls.L
			local fs = ls.fs
			local f = fs.f
			self:removevars(ls, 0)
			luaK:ret(fs, 0, 0)  -- final return
			-- luaM_reallocvector deleted for f->code, f->lineinfo, f->k, f->p,
			-- f->locvars, f->upvalues; not required for Lua table arrays
			f.sizecode = fs.pc
			f.sizelineinfo = fs.pc
			f.sizek = fs.nk
			f.sizep = fs.np
			f.sizelocvars = fs.nlocvars
			f.sizeupvalues = f.nups
			--lua_assert(luaG_checkcode(f))  -- currently not implemented
			lua_assert(fs.bl == nil)
			ls.fs = fs.prev
			-- the following is not required for this implementation; kept here
			-- for completeness
			-- L->top -= 2;  /* remove table and prototype from the stack */
			-- last token read was anchored in defunct function; must reanchor it
			if fs then self:anchor_token(ls) end
		end

		------------------------------------------------------------------------
		-- parser initialization function
		-- * note additional sub-tables needed for LexState, FuncState
		------------------------------------------------------------------------
		function luaY:parser(L, z, buff, name)
			local lexstate = {}  -- LexState
			lexstate.t = {}
			lexstate.lookahead = {}
			local funcstate = {}  -- FuncState
			funcstate.upvalues = {}
			funcstate.actvar = {}
			-- the following nCcalls initialization added for convenience
			L.nCcalls = 0
			lexstate.buff = buff
			luaX:setinput(L, lexstate, z, name)
			self:open_func(lexstate, funcstate)
			funcstate.f.is_vararg = self.VARARG_ISVARARG  -- main func. is always vararg
			luaX:next(lexstate)  -- read first token
			self:chunk(lexstate)
			self:check(lexstate, "TK_EOS")
			self:close_func(lexstate)
			lua_assert(funcstate.prev == nil)
			lua_assert(funcstate.f.nups == 0)
			lua_assert(lexstate.fs == nil)
			return funcstate.f
		end

--[[--------------------------------------------------------------------
-- GRAMMAR RULES
----------------------------------------------------------------------]]

		------------------------------------------------------------------------
		-- parse a function name suffix, for function call specifications
		-- * used in primaryexp(), funcname()
		------------------------------------------------------------------------
		function luaY:field(ls, v)
			-- field -> ['.' | ':'] NAME
			local fs = ls.fs
			local key = {}  -- expdesc
			luaK:exp2anyreg(fs, v)
			luaX:next(ls)  -- skip the dot or colon
			self:checkname(ls, key)
			luaK:indexed(fs, v, key)
		end

		------------------------------------------------------------------------
		-- parse a table indexing suffix, for constructors, expressions
		-- * used in recfield(), primaryexp()
		------------------------------------------------------------------------
		function luaY:yindex(ls, v)
			-- index -> '[' expr ']'
			luaX:next(ls)  -- skip the '['
			self:expr(ls, v)
			luaK:exp2val(ls.fs, v)
			self:checknext(ls, "]")
		end

--[[--------------------------------------------------------------------
-- Rules for Constructors
----------------------------------------------------------------------]]

--[[--------------------------------------------------------------------
-- struct ConsControl:
--   v  -- last list item read (table: struct expdesc)
--   t  -- table descriptor (table: struct expdesc)
--   nh  -- total number of 'record' elements
--   na  -- total number of array elements
--   tostore  -- number of array elements pending to be stored
----------------------------------------------------------------------]]

		------------------------------------------------------------------------
		-- parse a table record (hash) field
		-- * used in constructor()
		------------------------------------------------------------------------
		function luaY:recfield(ls, cc)
			-- recfield -> (NAME | '['exp1']') = exp1
			local fs = ls.fs
			local reg = ls.fs.freereg
			local key, val = {}, {}  -- expdesc
			if ls.t.token == "TK_NAME" then
				self:checklimit(fs, cc.nh, self.MAX_INT, "items in a constructor")
				self:checkname(ls, key)
			else  -- ls->t.token == '['
				self:yindex(ls, key)
			end
			cc.nh = cc.nh + 1
			self:checknext(ls, "=")
			local rkkey = luaK:exp2RK(fs, key)
			self:expr(ls, val)
			luaK:codeABC(fs, "OP_SETTABLE", cc.t.info, rkkey, luaK:exp2RK(fs, val))
			fs.freereg = reg  -- free registers
		end

		------------------------------------------------------------------------
		-- emit a set list instruction if enough elements (LFIELDS_PER_FLUSH)
		-- * used in constructor()
		------------------------------------------------------------------------
		function luaY:closelistfield(fs, cc)
			if cc.v.k == "VVOID" then return end  -- there is no list item
			luaK:exp2nextreg(fs, cc.v)
			cc.v.k = "VVOID"
			if cc.tostore == luaP.LFIELDS_PER_FLUSH then
				luaK:setlist(fs, cc.t.info, cc.na, cc.tostore)  -- flush
				cc.tostore = 0  -- no more items pending
			end
		end

		------------------------------------------------------------------------
		-- emit a set list instruction at the end of parsing list constructor
		-- * used in constructor()
		------------------------------------------------------------------------
		function luaY:lastlistfield(fs, cc)
			if cc.tostore == 0 then return end
			if self:hasmultret(cc.v.k) then
				luaK:setmultret(fs, cc.v)
				luaK:setlist(fs, cc.t.info, cc.na, self.LUA_MULTRET)
				cc.na = cc.na - 1  -- do not count last expression (unknown number of elements)
			else
				if cc.v.k ~= "VVOID" then
					luaK:exp2nextreg(fs, cc.v)
				end
				luaK:setlist(fs, cc.t.info, cc.na, cc.tostore)
			end
		end

		------------------------------------------------------------------------
		-- parse a table list (array) field
		-- * used in constructor()
		------------------------------------------------------------------------
		function luaY:listfield(ls, cc)
			self:expr(ls, cc.v)
			self:checklimit(ls.fs, cc.na, self.MAX_INT, "items in a constructor")
			cc.na = cc.na + 1
			cc.tostore = cc.tostore + 1
		end

		------------------------------------------------------------------------
		-- parse a table constructor
		-- * used in funcargs(), simpleexp()
		------------------------------------------------------------------------
		function luaY:constructor(ls, t)
			-- constructor -> '{' [ field { fieldsep field } [ fieldsep ] ] '}'
			-- field -> recfield | listfield
			-- fieldsep -> ',' | ';'
			local fs = ls.fs
			local line = ls.linenumber
			local pc = luaK:codeABC(fs, "OP_NEWTABLE", 0, 0, 0)
			local cc = {}  -- ConsControl
			cc.v = {}
			cc.na, cc.nh, cc.tostore = 0, 0, 0
			cc.t = t
			self:init_exp(t, "VRELOCABLE", pc)
			self:init_exp(cc.v, "VVOID", 0)  -- no value (yet)
			luaK:exp2nextreg(ls.fs, t)  -- fix it at stack top (for gc)
			self:checknext(ls, "{")
			repeat
				lua_assert(cc.v.k == "VVOID" or cc.tostore > 0)
				if ls.t.token == "}" then break end
				self:closelistfield(fs, cc)
				local c = ls.t.token

				if c == "TK_NAME" then  -- may be listfields or recfields
					luaX:lookahead(ls)
					if ls.lookahead.token ~= "=" then  -- expression?
						self:listfield(ls, cc)
					else
						self:recfield(ls, cc)
					end
				elseif c == "[" then  -- constructor_item -> recfield
					self:recfield(ls, cc)
				else  -- constructor_part -> listfield
					self:listfield(ls, cc)
				end
			until not self:testnext(ls, ",") and not self:testnext(ls, ";")
			self:check_match(ls, "}", "{", line)
			self:lastlistfield(fs, cc)
			luaP:SETARG_B(fs.f.code[pc], self:int2fb(cc.na)) -- set initial array size
			luaP:SETARG_C(fs.f.code[pc], self:int2fb(cc.nh)) -- set initial table size
		end

		-- }======================================================================

		------------------------------------------------------------------------
		-- parse the arguments (parameters) of a function declaration
		-- * used in body()
		------------------------------------------------------------------------
		function luaY:parlist(ls)
			-- parlist -> [ param { ',' param } ]
			local fs = ls.fs
			local f = fs.f
			local nparams = 0
			f.is_vararg = 0
			if ls.t.token ~= ")" then  -- is 'parlist' not empty?
				repeat
					local c = ls.t.token
					if c == "TK_NAME" then  -- param -> NAME
						self:new_localvar(ls, self:str_checkname(ls), nparams)
						nparams = nparams + 1
					elseif c == "TK_DOTS" then  -- param -> `...'
						luaX:next(ls)
						-- [[
						-- #if defined(LUA_COMPAT_VARARG)
						-- use `arg' as default name
						self:new_localvarliteral(ls, "arg", nparams)
						nparams = nparams + 1
						f.is_vararg = self.VARARG_HASARG + self.VARARG_NEEDSARG
						-- #endif
						--]]
						f.is_vararg = f.is_vararg + self.VARARG_ISVARARG
					else
						luaX:syntaxerror(ls, "<name> or "..self:LUA_QL("...").." expected")
					end
				until f.is_vararg ~= 0 or not self:testnext(ls, ",")
			end--if
			self:adjustlocalvars(ls, nparams)
			-- NOTE: the following works only when HASARG_MASK is 2!
			f.numparams = fs.nactvar - (f.is_vararg % self.HASARG_MASK)
			luaK:reserveregs(fs, fs.nactvar)  -- reserve register for parameters
		end

		------------------------------------------------------------------------
		-- parse function declaration body
		-- * used in simpleexp(), localfunc(), funcstat()
		------------------------------------------------------------------------
		function luaY:body(ls, e, needself, line)
			-- body ->  '(' parlist ')' chunk END
			local new_fs = {}  -- FuncState
			new_fs.upvalues = {}
			new_fs.actvar = {}
			self:open_func(ls, new_fs)
			new_fs.f.lineDefined = line
			self:checknext(ls, "(")
			if needself then
				self:new_localvarliteral(ls, "self", 0)
				self:adjustlocalvars(ls, 1)
			end
			self:parlist(ls)
			self:checknext(ls, ")")
			self:chunk(ls)
			new_fs.f.lastlinedefined = ls.linenumber
			self:check_match(ls, "TK_END", "TK_FUNCTION", line)
			self:close_func(ls)
			self:pushclosure(ls, new_fs, e)
		end

		------------------------------------------------------------------------
		-- parse a list of comma-separated expressions
		-- * used is multiple locations
		------------------------------------------------------------------------
		function luaY:explist1(ls, v)
			-- explist1 -> expr { ',' expr }
			local n = 1  -- at least one expression
			self:expr(ls, v)
			while self:testnext(ls, ",") do
				luaK:exp2nextreg(ls.fs, v)
				self:expr(ls, v)
				n = n + 1
			end
			return n
		end

		------------------------------------------------------------------------
		-- parse the parameters of a function call
		-- * contrast with parlist(), used in function declarations
		-- * used in primaryexp()
		------------------------------------------------------------------------
		function luaY:funcargs(ls, f)
			local fs = ls.fs
			local args = {}  -- expdesc
			local nparams
			local line = ls.linenumber
			local c = ls.t.token
			if c == "(" then  -- funcargs -> '(' [ explist1 ] ')'
				if line ~= ls.lastline then
					luaX:syntaxerror(ls, "ambiguous syntax (function call x new statement)")
				end
				luaX:next(ls)
				if ls.t.token == ")" then  -- arg list is empty?
					args.k = "VVOID"
				else
					self:explist1(ls, args)
					luaK:setmultret(fs, args)
				end
				self:check_match(ls, ")", "(", line)
			elseif c == "{" then  -- funcargs -> constructor
				self:constructor(ls, args)
			elseif c == "TK_STRING" then  -- funcargs -> STRING
				self:codestring(ls, args, ls.t.seminfo)
				luaX:next(ls)  -- must use 'seminfo' before 'next'
			else
				luaX:syntaxerror(ls, "function arguments expected")
				return
			end
			lua_assert(f.k == "VNONRELOC")
			local base = f.info  -- base register for call
			if self:hasmultret(args.k) then
				nparams = self.LUA_MULTRET  -- open call
			else
				if args.k ~= "VVOID" then
					luaK:exp2nextreg(fs, args)  -- close last argument
				end
				nparams = fs.freereg - (base + 1)
			end
			self:init_exp(f, "VCALL", luaK:codeABC(fs, "OP_CALL", base, nparams + 1, 2))
			luaK:fixline(fs, line)
			fs.freereg = base + 1  -- call remove function and arguments and leaves
			-- (unless changed) one result
		end

--[[--------------------------------------------------------------------
-- Expression parsing
----------------------------------------------------------------------]]

		------------------------------------------------------------------------
		-- parses an expression in parentheses or a single variable
		-- * used in primaryexp()
		------------------------------------------------------------------------
		function luaY:prefixexp(ls, v)
			-- prefixexp -> NAME | '(' expr ')'
			local c = ls.t.token
			if c == "(" then
				local line = ls.linenumber
				luaX:next(ls)
				self:expr(ls, v)
				self:check_match(ls, ")", "(", line)
				luaK:dischargevars(ls.fs, v)
			elseif c == "TK_NAME" then
				self:singlevar(ls, v)
			else
				luaX:syntaxerror(ls, "unexpected symbol")
			end--if c
			return
		end

		------------------------------------------------------------------------
		-- parses a prefixexp (an expression in parentheses or a single variable)
		-- or a function call specification
		-- * used in simpleexp(), assignment(), exprstat()
		------------------------------------------------------------------------
		function luaY:primaryexp(ls, v)
			-- primaryexp ->
			--    prefixexp { '.' NAME | '[' exp ']' | ':' NAME funcargs | funcargs }
			local fs = ls.fs
			self:prefixexp(ls, v)
			while true do
				local c = ls.t.token
				if c == "." then  -- field
					self:field(ls, v)
				elseif c == "[" then  -- '[' exp1 ']'
					local key = {}  -- expdesc
					luaK:exp2anyreg(fs, v)
					self:yindex(ls, key)
					luaK:indexed(fs, v, key)
				elseif c == ":" then  -- ':' NAME funcargs
					local key = {}  -- expdesc
					luaX:next(ls)
					self:checkname(ls, key)
					luaK:_self(fs, v, key)
					self:funcargs(ls, v)
				elseif c == "(" or c == "TK_STRING" or c == "{" then  -- funcargs
					luaK:exp2nextreg(fs, v)
					self:funcargs(ls, v)
				else
					return
				end--if c
			end--while
		end

		------------------------------------------------------------------------
		-- parses general expression types, constants handled here
		-- * used in subexpr()
		------------------------------------------------------------------------
		function luaY:simpleexp(ls, v)
			-- simpleexp -> NUMBER | STRING | NIL | TRUE | FALSE | ... |
			--              constructor | FUNCTION body | primaryexp
			local c = ls.t.token
			if c == "TK_NUMBER" then
				self:init_exp(v, "VKNUM", 0)
				v.nval = ls.t.seminfo
			elseif c == "TK_STRING" then
				self:codestring(ls, v, ls.t.seminfo)
			elseif c == "TK_NIL" then
				self:init_exp(v, "VNIL", 0)
			elseif c == "TK_TRUE" then
				self:init_exp(v, "VTRUE", 0)
			elseif c == "TK_FALSE" then
				self:init_exp(v, "VFALSE", 0)
			elseif c == "TK_DOTS" then  -- vararg
				local fs = ls.fs
				self:check_condition(ls, fs.f.is_vararg ~= 0,
					"cannot use "..self:LUA_QL("...").." outside a vararg function");
				-- NOTE: the following substitutes for a bitop, but is value-specific
				local is_vararg = fs.f.is_vararg
				if is_vararg >= self.VARARG_NEEDSARG then
					fs.f.is_vararg = is_vararg - self.VARARG_NEEDSARG  -- don't need 'arg'
				end
				self:init_exp(v, "VVARARG", luaK:codeABC(fs, "OP_VARARG", 0, 1, 0))
			elseif c == "{" then  -- constructor
				self:constructor(ls, v)
				return
			elseif c == "TK_FUNCTION" then
				luaX:next(ls)
				self:body(ls, v, false, ls.linenumber)
				return
			else
				self:primaryexp(ls, v)
				return
			end--if c
			luaX:next(ls)
		end

		------------------------------------------------------------------------
		-- Translates unary operators tokens if found, otherwise returns
		-- OPR_NOUNOPR. getunopr() and getbinopr() are used in subexpr().
		-- * used in subexpr()
		------------------------------------------------------------------------
		function luaY:getunopr(op)
			if op == "TK_NOT" then
				return "OPR_NOT"
			elseif op == "-" then
				return "OPR_MINUS"
			elseif op == "#" then
				return "OPR_LEN"
			else
				return "OPR_NOUNOPR"
			end
		end

		------------------------------------------------------------------------
		-- Translates binary operator tokens if found, otherwise returns
		-- OPR_NOBINOPR. Code generation uses OPR_* style tokens.
		-- * used in subexpr()
		------------------------------------------------------------------------
		luaY.getbinopr_table = {
			["+"] = "OPR_ADD",
			["-"] = "OPR_SUB",
			["*"] = "OPR_MUL",
			["/"] = "OPR_DIV",
			["%"] = "OPR_MOD",
			["^"] = "OPR_POW",
			["TK_CONCAT"] = "OPR_CONCAT",
			["TK_NE"] = "OPR_NE",
			["TK_EQ"] = "OPR_EQ",
			["<"] = "OPR_LT",
			["TK_LE"] = "OPR_LE",
			[">"] = "OPR_GT",
			["TK_GE"] = "OPR_GE",
			["TK_AND"] = "OPR_AND",
			["TK_OR"] = "OPR_OR",
		}
		function luaY:getbinopr(op)
			local opr = self.getbinopr_table[op]
			if opr then return opr else return "OPR_NOBINOPR" end
		end

		------------------------------------------------------------------------
		-- the following priority table consists of pairs of left/right values
		-- for binary operators (was a static const struct); grep for ORDER OPR
		-- * the following struct is replaced:
		--   static const struct {
		--     lu_byte left;  /* left priority for each binary operator */
		--     lu_byte right; /* right priority */
		--   } priority[] = {  /* ORDER OPR */
		------------------------------------------------------------------------
		luaY.priority = {
			{6, 6}, {6, 6}, {7, 7}, {7, 7}, {7, 7}, -- `+' `-' `/' `%'
			{10, 9}, {5, 4},                 -- power and concat (right associative)
			{3, 3}, {3, 3},                  -- equality
			{3, 3}, {3, 3}, {3, 3}, {3, 3},  -- order
			{2, 2}, {1, 1}                   -- logical (and/or)
		}

		luaY.UNARY_PRIORITY = 8  -- priority for unary operators

		------------------------------------------------------------------------
		-- Parse subexpressions. Includes handling of unary operators and binary
		-- operators. A subexpr is given the rhs priority level of the operator
		-- immediately left of it, if any (limit is -1 if none,) and if a binop
		-- is found, limit is compared with the lhs priority level of the binop
		-- in order to determine which executes first.
		------------------------------------------------------------------------

		------------------------------------------------------------------------
		-- subexpr -> (simpleexp | unop subexpr) { binop subexpr }
		-- where 'binop' is any binary operator with a priority higher than 'limit'
		-- * for priority lookups with self.priority[], 1=left and 2=right
		-- * recursively called
		-- * used in expr()
		------------------------------------------------------------------------
		function luaY:subexpr(ls, v, limit)
			self:enterlevel(ls)
			local uop = self:getunopr(ls.t.token)
			if uop ~= "OPR_NOUNOPR" then
				luaX:next(ls)
				self:subexpr(ls, v, self.UNARY_PRIORITY)
				luaK:prefix(ls.fs, uop, v)
			else
				self:simpleexp(ls, v)
			end
			-- expand while operators have priorities higher than 'limit'
			local op = self:getbinopr(ls.t.token)
			while op ~= "OPR_NOBINOPR" and self.priority[luaK.BinOpr[op] + 1][1] > limit do
				local v2 = {}  -- expdesc
				luaX:next(ls)
				luaK:infix(ls.fs, op, v)
				-- read sub-expression with higher priority
				local nextop = self:subexpr(ls, v2, self.priority[luaK.BinOpr[op] + 1][2])
				luaK:posfix(ls.fs, op, v, v2)
				op = nextop
			end
			self:leavelevel(ls)
			return op  -- return first untreated operator
		end

		------------------------------------------------------------------------
		-- Expression parsing starts here. Function subexpr is entered with the
		-- left operator (which is non-existent) priority of -1, which is lower
		-- than all actual operators. Expr information is returned in parm v.
		-- * used in multiple locations
		------------------------------------------------------------------------
		function luaY:expr(ls, v)
			self:subexpr(ls, v, 0)
		end

		-- }====================================================================

--[[--------------------------------------------------------------------
-- Rules for Statements
----------------------------------------------------------------------]]

		------------------------------------------------------------------------
		-- checks next token, used as a look-ahead
		-- * returns boolean instead of 0|1
		-- * used in retstat(), chunk()
		------------------------------------------------------------------------
		function luaY:block_follow(token)
			if token == "TK_ELSE" or token == "TK_ELSEIF" or token == "TK_END"
				or token == "TK_UNTIL" or token == "TK_EOS" then
				return true
			else
				return false
			end
		end

		------------------------------------------------------------------------
		-- parse a code block or unit
		-- * used in multiple functions
		------------------------------------------------------------------------
		function luaY:block(ls)
			-- block -> chunk
			local fs = ls.fs
			local bl = {}  -- BlockCnt
			self:enterblock(fs, bl, false)
			self:chunk(ls)
			lua_assert(bl.breaklist == luaK.NO_JUMP)
			self:leaveblock(fs)
		end

		------------------------------------------------------------------------
		-- structure to chain all variables in the left-hand side of an
		-- assignment
		-- struct LHS_assign:
		--   prev  -- (table: struct LHS_assign)
		--   v  -- variable (global, local, upvalue, or indexed) (table: expdesc)
		------------------------------------------------------------------------

		------------------------------------------------------------------------
		-- check whether, in an assignment to a local variable, the local variable
		-- is needed in a previous assignment (to a table). If so, save original
		-- local value in a safe place and use this safe copy in the previous
		-- assignment.
		-- * used in assignment()
		------------------------------------------------------------------------
		function luaY:check_conflict(ls, lh, v)
			local fs = ls.fs
			local extra = fs.freereg  -- eventual position to save local variable
			local conflict = false
			while lh do
				if lh.v.k == "VINDEXED" then
					if lh.v.info == v.info then  -- conflict?
						conflict = true
						lh.v.info = extra  -- previous assignment will use safe copy
					end
					if lh.v.aux == v.info then  -- conflict?
						conflict = true
						lh.v.aux = extra  -- previous assignment will use safe copy
					end
				end
				lh = lh.prev
			end
			if conflict then
				luaK:codeABC(fs, "OP_MOVE", fs.freereg, v.info, 0)  -- make copy
				luaK:reserveregs(fs, 1)
			end
		end

		------------------------------------------------------------------------
		-- parse a variable assignment sequence
		-- * recursively called
		-- * used in exprstat()
		------------------------------------------------------------------------
		function luaY:assignment(ls, lh, nvars)
			local e = {}  -- expdesc
			-- test was: VLOCAL <= lh->v.k && lh->v.k <= VINDEXED
			local c = lh.v.k
			self:check_condition(ls, c == "VLOCAL" or c == "VUPVAL" or c == "VGLOBAL"
				or c == "VINDEXED", "syntax error")
			if self:testnext(ls, ",") then  -- assignment -> ',' primaryexp assignment
				local nv = {}  -- LHS_assign
				nv.v = {}
				nv.prev = lh
				self:primaryexp(ls, nv.v)
				if nv.v.k == "VLOCAL" then
					self:check_conflict(ls, lh, nv.v)
				end
				self:checklimit(ls.fs, nvars, self.LUAI_MAXCCALLS - ls.L.nCcalls,
					"variables in assignment")
				self:assignment(ls, nv, nvars + 1)
			else  -- assignment -> '=' explist1
				self:checknext(ls, "=")
				local nexps = self:explist1(ls, e)
				if nexps ~= nvars then
					self:adjust_assign(ls, nvars, nexps, e)
					if nexps > nvars then
						ls.fs.freereg = ls.fs.freereg - (nexps - nvars)  -- remove extra values
					end
				else
					luaK:setoneret(ls.fs, e)  -- close last expression
					luaK:storevar(ls.fs, lh.v, e)
					return  -- avoid default
				end
			end
			self:init_exp(e, "VNONRELOC", ls.fs.freereg - 1)  -- default assignment
			luaK:storevar(ls.fs, lh.v, e)
		end

		------------------------------------------------------------------------
		-- parse condition in a repeat statement or an if control structure
		-- * used in repeatstat(), test_then_block()
		------------------------------------------------------------------------
		function luaY:cond(ls)
			-- cond -> exp
			local v = {}  -- expdesc
			self:expr(ls, v)  -- read condition
			if v.k == "VNIL" then v.k = "VFALSE" end  -- 'falses' are all equal here
			luaK:goiftrue(ls.fs, v)
			return v.f
		end

		------------------------------------------------------------------------
		-- parse a break statement
		-- * used in statements()
		------------------------------------------------------------------------
		function luaY:breakstat(ls)
			-- stat -> BREAK
			local fs = ls.fs
			local bl = fs.bl
			local upval = false
			while bl and not bl.isbreakable do
				if bl.upval then upval = true end
				bl = bl.previous
			end
			if not bl then
				luaX:syntaxerror(ls, "no loop to break")
			end
			if upval then
				luaK:codeABC(fs, "OP_CLOSE", bl.nactvar, 0, 0)
			end
			bl.breaklist = luaK:concat(fs, bl.breaklist, luaK:jump(fs))
		end

		------------------------------------------------------------------------
		-- parse a while-do control structure, body processed by block()
		-- * with dynamic array sizes, MAXEXPWHILE + EXTRAEXP limits imposed by
		--   the function's implementation can be removed
		-- * used in statements()
		------------------------------------------------------------------------
		function luaY:whilestat(ls, line)
			-- whilestat -> WHILE cond DO block END
			local fs = ls.fs
			local bl = {}  -- BlockCnt
			luaX:next(ls)  -- skip WHILE
			local whileinit = luaK:getlabel(fs)
			local condexit = self:cond(ls)
			self:enterblock(fs, bl, true)
			self:checknext(ls, "TK_DO")
			self:block(ls)
			luaK:patchlist(fs, luaK:jump(fs), whileinit)
			self:check_match(ls, "TK_END", "TK_WHILE", line)
			self:leaveblock(fs)
			luaK:patchtohere(fs, condexit)  -- false conditions finish the loop
		end

		------------------------------------------------------------------------
		-- parse a repeat-until control structure, body parsed by chunk()
		-- * used in statements()
		------------------------------------------------------------------------
		function luaY:repeatstat(ls, line)
			-- repeatstat -> REPEAT block UNTIL cond
			local fs = ls.fs
			local repeat_init = luaK:getlabel(fs)
			local bl1, bl2 = {}, {}  -- BlockCnt
			self:enterblock(fs, bl1, true)  -- loop block
			self:enterblock(fs, bl2, false)  -- scope block
			luaX:next(ls)  -- skip REPEAT
			self:chunk(ls)
			self:check_match(ls, "TK_UNTIL", "TK_REPEAT", line)
			local condexit = self:cond(ls)  -- read condition (inside scope block)
			if not bl2.upval then  -- no upvalues?
				self:leaveblock(fs)  -- finish scope
				luaK:patchlist(ls.fs, condexit, repeat_init)  -- close the loop
			else  -- complete semantics when there are upvalues
				self:breakstat(ls)  -- if condition then break
				luaK:patchtohere(ls.fs, condexit)  -- else...
				self:leaveblock(fs)  -- finish scope...
				luaK:patchlist(ls.fs, luaK:jump(fs), repeat_init)  -- and repeat
			end
			self:leaveblock(fs)  -- finish loop
		end

		------------------------------------------------------------------------
		-- parse the single expressions needed in numerical for loops
		-- * used in fornum()
		------------------------------------------------------------------------
		function luaY:exp1(ls)
			local e = {}  -- expdesc
			self:expr(ls, e)
			local k = e.k
			luaK:exp2nextreg(ls.fs, e)
			return k
		end

		------------------------------------------------------------------------
		-- parse a for loop body for both versions of the for loop
		-- * used in fornum(), forlist()
		------------------------------------------------------------------------
		function luaY:forbody(ls, base, line, nvars, isnum)
			-- forbody -> DO block
			local bl = {}  -- BlockCnt
			local fs = ls.fs
			self:adjustlocalvars(ls, 3)  -- control variables
			self:checknext(ls, "TK_DO")
			local prep = isnum and luaK:codeAsBx(fs, "OP_FORPREP", base, luaK.NO_JUMP)
				or luaK:jump(fs)
			self:enterblock(fs, bl, false)  -- scope for declared variables
			self:adjustlocalvars(ls, nvars)
			luaK:reserveregs(fs, nvars)
			self:block(ls)
			self:leaveblock(fs)  -- end of scope for declared variables
			luaK:patchtohere(fs, prep)
			local endfor = isnum and luaK:codeAsBx(fs, "OP_FORLOOP", base, luaK.NO_JUMP)
				or luaK:codeABC(fs, "OP_TFORLOOP", base, 0, nvars)
			luaK:fixline(fs, line)  -- pretend that `OP_FOR' starts the loop
			luaK:patchlist(fs, isnum and endfor or luaK:jump(fs), prep + 1)
		end

		------------------------------------------------------------------------
		-- parse a numerical for loop, calls forbody()
		-- * used in forstat()
		------------------------------------------------------------------------
		function luaY:fornum(ls, varname, line)
			-- fornum -> NAME = exp1,exp1[,exp1] forbody
			local fs = ls.fs
			local base = fs.freereg
			self:new_localvarliteral(ls, "(for index)", 0)
			self:new_localvarliteral(ls, "(for limit)", 1)
			self:new_localvarliteral(ls, "(for step)", 2)
			self:new_localvar(ls, varname, 3)
			self:checknext(ls, '=')
			self:exp1(ls)  -- initial value
			self:checknext(ls, ",")
			self:exp1(ls)  -- limit
			if self:testnext(ls, ",") then
				self:exp1(ls)  -- optional step
			else  -- default step = 1
				luaK:codeABx(fs, "OP_LOADK", fs.freereg, luaK:numberK(fs, 1))
				luaK:reserveregs(fs, 1)
			end
			self:forbody(ls, base, line, 1, true)
		end

		------------------------------------------------------------------------
		-- parse a generic for loop, calls forbody()
		-- * used in forstat()
		------------------------------------------------------------------------
		function luaY:forlist(ls, indexname)
			-- forlist -> NAME {,NAME} IN explist1 forbody
			local fs = ls.fs
			local e = {}  -- expdesc
			local nvars = 0
			local base = fs.freereg
			-- create control variables
			self:new_localvarliteral(ls, "(for generator)", nvars)
			nvars = nvars + 1
			self:new_localvarliteral(ls, "(for state)", nvars)
			nvars = nvars + 1
			self:new_localvarliteral(ls, "(for control)", nvars)
			nvars = nvars + 1
			-- create declared variables
			self:new_localvar(ls, indexname, nvars)
			nvars = nvars + 1
			while self:testnext(ls, ",") do
				self:new_localvar(ls, self:str_checkname(ls), nvars)
				nvars = nvars + 1
			end
			self:checknext(ls, "TK_IN")
			local line = ls.linenumber
			self:adjust_assign(ls, 3, self:explist1(ls, e), e)
			luaK:checkstack(fs, 3)  -- extra space to call generator
			self:forbody(ls, base, line, nvars - 3, false)
		end

		------------------------------------------------------------------------
		-- initial parsing for a for loop, calls fornum() or forlist()
		-- * used in statements()
		------------------------------------------------------------------------
		function luaY:forstat(ls, line)
			-- forstat -> FOR (fornum | forlist) END
			local fs = ls.fs
			local bl = {}  -- BlockCnt
			self:enterblock(fs, bl, true)  -- scope for loop and control variables
			luaX:next(ls)  -- skip `for'
			local varname = self:str_checkname(ls)  -- first variable name
			local c = ls.t.token
			if c == "=" then
				self:fornum(ls, varname, line)
			elseif c == "," or c == "TK_IN" then
				self:forlist(ls, varname)
			else
				luaX:syntaxerror(ls, self:LUA_QL("=").." or "..self:LUA_QL("in").." expected")
			end
			self:check_match(ls, "TK_END", "TK_FOR", line)
			self:leaveblock(fs)  -- loop scope (`break' jumps to this point)
		end

		------------------------------------------------------------------------
		-- parse part of an if control structure, including the condition
		-- * used in ifstat()
		------------------------------------------------------------------------
		function luaY:test_then_block(ls)
			-- test_then_block -> [IF | ELSEIF] cond THEN block
			luaX:next(ls)  -- skip IF or ELSEIF
			local condexit = self:cond(ls)
			self:checknext(ls, "TK_THEN")
			self:block(ls)  -- `then' part
			return condexit
		end

		------------------------------------------------------------------------
		-- parse an if control structure
		-- * used in statements()
		------------------------------------------------------------------------
		function luaY:ifstat(ls, line)
			-- ifstat -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END
			local fs = ls.fs
			local escapelist = luaK.NO_JUMP
			local flist = self:test_then_block(ls)  -- IF cond THEN block
			while ls.t.token == "TK_ELSEIF" do
				escapelist = luaK:concat(fs, escapelist, luaK:jump(fs))
				luaK:patchtohere(fs, flist)
				flist = self:test_then_block(ls)  -- ELSEIF cond THEN block
			end
			if ls.t.token == "TK_ELSE" then
				escapelist = luaK:concat(fs, escapelist, luaK:jump(fs))
				luaK:patchtohere(fs, flist)
				luaX:next(ls)  -- skip ELSE (after patch, for correct line info)
				self:block(ls)  -- 'else' part
			else
				escapelist = luaK:concat(fs, escapelist, flist)
			end
			luaK:patchtohere(fs, escapelist)
			self:check_match(ls, "TK_END", "TK_IF", line)
		end

		------------------------------------------------------------------------
		-- parse a local function statement
		-- * used in statements()
		------------------------------------------------------------------------
		function luaY:localfunc(ls)
			local v, b = {}, {}  -- expdesc
			local fs = ls.fs
			self:new_localvar(ls, self:str_checkname(ls), 0)
			self:init_exp(v, "VLOCAL", fs.freereg)
			luaK:reserveregs(fs, 1)
			self:adjustlocalvars(ls, 1)
			self:body(ls, b, false, ls.linenumber)
			luaK:storevar(fs, v, b)
			-- debug information will only see the variable after this point!
			self:getlocvar(fs, fs.nactvar - 1).startpc = fs.pc
		end

		------------------------------------------------------------------------
		-- parse a local variable declaration statement
		-- * used in statements()
		------------------------------------------------------------------------
		function luaY:localstat(ls)
			-- stat -> LOCAL NAME {',' NAME} ['=' explist1]
			local nvars = 0
			local nexps
			local e = {}  -- expdesc
			repeat
				self:new_localvar(ls, self:str_checkname(ls), nvars)
				nvars = nvars + 1
			until not self:testnext(ls, ",")
			if self:testnext(ls, "=") then
				nexps = self:explist1(ls, e)
			else
				e.k = "VVOID"
				nexps = 0
			end
			self:adjust_assign(ls, nvars, nexps, e)
			self:adjustlocalvars(ls, nvars)
		end

		------------------------------------------------------------------------
		-- parse a function name specification
		-- * used in funcstat()
		------------------------------------------------------------------------
		function luaY:funcname(ls, v)
			-- funcname -> NAME {field} [':' NAME]
			local needself = false
			self:singlevar(ls, v)
			while ls.t.token == "." do
				self:field(ls, v)
			end
			if ls.t.token == ":" then
				needself = true
				self:field(ls, v)
			end
			return needself
		end

		------------------------------------------------------------------------
		-- parse a function statement
		-- * used in statements()
		------------------------------------------------------------------------
		function luaY:funcstat(ls, line)
			-- funcstat -> FUNCTION funcname body
			local v, b = {}, {}  -- expdesc
			luaX:next(ls)  -- skip FUNCTION
			local needself = self:funcname(ls, v)
			self:body(ls, b, needself, line)
			luaK:storevar(ls.fs, v, b)
			luaK:fixline(ls.fs, line)  -- definition 'happens' in the first line
		end

		------------------------------------------------------------------------
		-- parse a function call with no returns or an assignment statement
		-- * used in statements()
		------------------------------------------------------------------------
		function luaY:exprstat(ls)
			-- stat -> func | assignment
			local fs = ls.fs
			local v = {}  -- LHS_assign
			v.v = {}
			self:primaryexp(ls, v.v)
			if v.v.k == "VCALL" then  -- stat -> func
				luaP:SETARG_C(luaK:getcode(fs, v.v), 1)  -- call statement uses no results
			else  -- stat -> assignment
				v.prev = nil
				self:assignment(ls, v, 1)
			end
		end

		------------------------------------------------------------------------
		-- parse a return statement
		-- * used in statements()
		------------------------------------------------------------------------
		function luaY:retstat(ls)
			-- stat -> RETURN explist
			local fs = ls.fs
			local e = {}  -- expdesc
			local first, nret  -- registers with returned values
			luaX:next(ls)  -- skip RETURN
			if self:block_follow(ls.t.token) or ls.t.token == ";" then
				first, nret = 0, 0  -- return no values
			else
				nret = self:explist1(ls, e)  -- optional return values
				if self:hasmultret(e.k) then
					luaK:setmultret(fs, e)
					if e.k == "VCALL" and nret == 1 then  -- tail call?
						luaP:SET_OPCODE(luaK:getcode(fs, e), "OP_TAILCALL")
						lua_assert(luaP:GETARG_A(luaK:getcode(fs, e)) == fs.nactvar)
					end
					first = fs.nactvar
					nret = self.LUA_MULTRET  -- return all values
				else
					if nret == 1 then  -- only one single value?
						first = luaK:exp2anyreg(fs, e)
					else
						luaK:exp2nextreg(fs, e)  -- values must go to the 'stack'
						first = fs.nactvar  -- return all 'active' values
						lua_assert(nret == fs.freereg - first)
					end
				end--if
			end--if
			luaK:ret(fs, first, nret)
		end

		------------------------------------------------------------------------
		-- initial parsing for statements, calls a lot of functions
		-- * returns boolean instead of 0|1
		-- * used in chunk()
		------------------------------------------------------------------------
		function luaY:statement(ls)
			local line = ls.linenumber  -- may be needed for error messages
			local c = ls.t.token
			if c == "TK_IF" then  -- stat -> ifstat
				self:ifstat(ls, line)
				return false
			elseif c == "TK_WHILE" then  -- stat -> whilestat
				self:whilestat(ls, line)
				return false
			elseif c == "TK_DO" then  -- stat -> DO block END
				luaX:next(ls)  -- skip DO
				self:block(ls)
				self:check_match(ls, "TK_END", "TK_DO", line)
				return false
			elseif c == "TK_FOR" then  -- stat -> forstat
				self:forstat(ls, line)
				return false
			elseif c == "TK_REPEAT" then  -- stat -> repeatstat
				self:repeatstat(ls, line)
				return false
			elseif c == "TK_FUNCTION" then  -- stat -> funcstat
				self:funcstat(ls, line)
				return false
			elseif c == "TK_LOCAL" then  -- stat -> localstat
				luaX:next(ls)  -- skip LOCAL
				if self:testnext(ls, "TK_FUNCTION") then  -- local function?
					self:localfunc(ls)
				else
					self:localstat(ls)
				end
				return false
			elseif c == "TK_RETURN" then  -- stat -> retstat
				self:retstat(ls)
				return true  -- must be last statement
			elseif c == "TK_BREAK" then  -- stat -> breakstat
				luaX:next(ls)  -- skip BREAK
				self:breakstat(ls)
				return true  -- must be last statement
			else
				self:exprstat(ls)
				return false  -- to avoid warnings
			end--if c
		end

		------------------------------------------------------------------------
		-- parse a chunk, which consists of a bunch of statements
		-- * used in parser(), body(), block(), repeatstat()
		------------------------------------------------------------------------
		function luaY:chunk(ls)
			-- chunk -> { stat [';'] }
			local islast = false
			self:enterlevel(ls)
			while not islast and not self:block_follow(ls.t.token) do
				islast = self:statement(ls)
				self:testnext(ls, ";")
				lua_assert(ls.fs.f.maxstacksize >= ls.fs.freereg and
					ls.fs.freereg >= ls.fs.nactvar)
				ls.fs.freereg = ls.fs.nactvar  -- free registers
			end
			self:leavelevel(ls)
		end

		-- }======================================================================





		luaX:init()  -- required by llex
		local LuaState = {}  -- dummy, not actually used, but retained since
		-- the intention is to complete a straight port

		------------------------------------------------------------------------
		-- interfacing to yueliang
		------------------------------------------------------------------------


		return function (source, name)
			name = name or 'compiled-lua'
			-- luaZ:make_getF returns a file chunk reader
			-- luaZ:init returns a zio input stream
			local zio = luaZ:init(luaZ:make_getF(source), nil)
			if not zio then return end
			-- luaY:parser parses the input stream
			-- func is the function prototype in tabular form; in C, func can
			-- now be used directly by the VM, this can't be done in Lua

			local func = luaY:parser(LuaState, zio, nil, "@"..name)
			-- luaU:make_setS returns a string chunk writer
			local writer, buff = luaU:make_setS()
			-- luaU:dump builds a binary chunk
			luaU:dump(LuaState, func, writer, buff)
			-- a string.dump equivalent in returned

			return buff.data
		end
	end)()

	local createExecutable = coroutine.wrap(function()
    --[[
FiOne
Copyright (C) 2021  Rerumu

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
]] --
		local bit = bit or bit32 or require('bit')

		if not table.create then function table.create(_) return {} end end

		if not table.unpack then table.unpack = unpack end

		if not table.pack then function table.pack(...) return {n = select('#', ...), ...} end end

		if not table.move then
			function table.move(src, first, last, offset, dst)
				for i = 0, last - first do dst[offset + i] = src[first + i] end
			end
		end

		local lua_bc_to_state
		local lua_wrap_state
		local stm_lua_func

		-- SETLIST config
		local FIELDS_PER_FLUSH = 50

		-- remap for better lookup
		local OPCODE_RM = {
			-- level 1
			[22] = 18, -- JMP
			[31] = 8, -- FORLOOP
			[33] = 28, -- TFORLOOP
			-- level 2
			[0] = 3, -- MOVE
			[1] = 13, -- LOADK
			[2] = 23, -- LOADBOOL
			[26] = 33, -- TEST
			-- level 3
			[12] = 1, -- ADD
			[13] = 6, -- SUB
			[14] = 10, -- MUL
			[15] = 16, -- DIV
			[16] = 20, -- MOD
			[17] = 26, -- POW
			[18] = 30, -- UNM
			[19] = 36, -- NOT
			-- level 4
			[3] = 0, -- LOADNIL
			[4] = 2, -- GETUPVAL
			[5] = 4, -- GETGLOBAL
			[6] = 7, -- GETTABLE
			[7] = 9, -- SETGLOBAL
			[8] = 12, -- SETUPVAL
			[9] = 14, -- SETTABLE
			[10] = 17, -- NEWTABLE
			[20] = 19, -- LEN
			[21] = 22, -- CONCAT
			[23] = 24, -- EQ
			[24] = 27, -- LT
			[25] = 29, -- LE
			[27] = 32, -- TESTSET
			[32] = 34, -- FORPREP
			[34] = 37, -- SETLIST
			-- level 5
			[11] = 5, -- SELF
			[28] = 11, -- CALL
			[29] = 15, -- TAILCALL
			[30] = 21, -- RETURN
			[35] = 25, -- CLOSE
			[36] = 31, -- CLOSURE
			[37] = 35, -- VARARG
		}

		-- opcode types for getting values
		local OPCODE_T = {
			[0] = 'ABC',
			'ABx',
			'ABC',
			'ABC',
			'ABC',
			'ABx',
			'ABC',
			'ABx',
			'ABC',
			'ABC',
			'ABC',
			'ABC',
			'ABC',
			'ABC',
			'ABC',
			'ABC',
			'ABC',
			'ABC',
			'ABC',
			'ABC',
			'ABC',
			'ABC',
			'AsBx',
			'ABC',
			'ABC',
			'ABC',
			'ABC',
			'ABC',
			'ABC',
			'ABC',
			'ABC',
			'AsBx',
			'AsBx',
			'ABC',
			'ABC',
			'ABC',
			'ABx',
			'ABC',
		}

		local OPCODE_M = {
			[0] = {b = 'OpArgR', c = 'OpArgN'},
			{b = 'OpArgK', c = 'OpArgN'},
			{b = 'OpArgU', c = 'OpArgU'},
			{b = 'OpArgR', c = 'OpArgN'},
			{b = 'OpArgU', c = 'OpArgN'},
			{b = 'OpArgK', c = 'OpArgN'},
			{b = 'OpArgR', c = 'OpArgK'},
			{b = 'OpArgK', c = 'OpArgN'},
			{b = 'OpArgU', c = 'OpArgN'},
			{b = 'OpArgK', c = 'OpArgK'},
			{b = 'OpArgU', c = 'OpArgU'},
			{b = 'OpArgR', c = 'OpArgK'},
			{b = 'OpArgK', c = 'OpArgK'},
			{b = 'OpArgK', c = 'OpArgK'},
			{b = 'OpArgK', c = 'OpArgK'},
			{b = 'OpArgK', c = 'OpArgK'},
			{b = 'OpArgK', c = 'OpArgK'},
			{b = 'OpArgK', c = 'OpArgK'},
			{b = 'OpArgR', c = 'OpArgN'},
			{b = 'OpArgR', c = 'OpArgN'},
			{b = 'OpArgR', c = 'OpArgN'},
			{b = 'OpArgR', c = 'OpArgR'},
			{b = 'OpArgR', c = 'OpArgN'},
			{b = 'OpArgK', c = 'OpArgK'},
			{b = 'OpArgK', c = 'OpArgK'},
			{b = 'OpArgK', c = 'OpArgK'},
			{b = 'OpArgR', c = 'OpArgU'},
			{b = 'OpArgR', c = 'OpArgU'},
			{b = 'OpArgU', c = 'OpArgU'},
			{b = 'OpArgU', c = 'OpArgU'},
			{b = 'OpArgU', c = 'OpArgN'},
			{b = 'OpArgR', c = 'OpArgN'},
			{b = 'OpArgR', c = 'OpArgN'},
			{b = 'OpArgN', c = 'OpArgU'},
			{b = 'OpArgU', c = 'OpArgU'},
			{b = 'OpArgN', c = 'OpArgN'},
			{b = 'OpArgU', c = 'OpArgN'},
			{b = 'OpArgU', c = 'OpArgN'},
		}

		-- int rd_int_basic(string src, int s, int e, int d)
		-- @src - Source binary string
		-- @s - Start index of a little endian integer
		-- @e - End index of the integer
		-- @d - Direction of the loop
		local function rd_int_basic(src, s, e, d)
			local num = 0

			-- if bb[l] > 127 then -- signed negative
			-- 	num = num - 256 ^ l
			-- 	bb[l] = bb[l] - 128
			-- end

			for i = s, e, d do
				local mul = 256 ^ math.abs(i - s)

				num = num + mul * string.byte(src, i, i)
			end

			return num
		end

		-- float rd_flt_basic(byte f1..8)
		-- @f1..4 - The 4 bytes composing a little endian float
		local function rd_flt_basic(f1, f2, f3, f4)
			local sign = (-1) ^ bit.rshift(f4, 7)
			local exp = bit.rshift(f3, 7) + bit.lshift(bit.band(f4, 0x7F), 1)
			local frac = f1 + bit.lshift(f2, 8) + bit.lshift(bit.band(f3, 0x7F), 16)
			local normal = 1

			if exp == 0 then
				if frac == 0 then
					return sign * 0
				else
					normal = 0
					exp = 1
				end
			elseif exp == 0x7F then
				if frac == 0 then
					return sign * (1 / 0)
				else
					return sign * (0 / 0)
				end
			end

			return sign * 2 ^ (exp - 127) * (1 + normal / 2 ^ 23)
		end

		-- double rd_dbl_basic(byte f1..8)
		-- @f1..8 - The 8 bytes composing a little endian double
		local function rd_dbl_basic(f1, f2, f3, f4, f5, f6, f7, f8)
			local sign = (-1) ^ bit.rshift(f8, 7)
			local exp = bit.lshift(bit.band(f8, 0x7F), 4) + bit.rshift(f7, 4)
			local frac = bit.band(f7, 0x0F) * 2 ^ 48
			local normal = 1

			frac = frac + (f6 * 2 ^ 40) + (f5 * 2 ^ 32) + (f4 * 2 ^ 24) + (f3 * 2 ^ 16) + (f2 * 2 ^ 8) + f1 -- help

			if exp == 0 then
				if frac == 0 then
					return sign * 0
				else
					normal = 0
					exp = 1
				end
			elseif exp == 0x7FF then
				if frac == 0 then
					return sign * (1 / 0)
				else
					return sign * (0 / 0)
				end
			end

			return sign * 2 ^ (exp - 1023) * (normal + frac / 2 ^ 52)
		end

		-- int rd_int_le(string src, int s, int e)
		-- @src - Source binary string
		-- @s - Start index of a little endian integer
		-- @e - End index of the integer
		local function rd_int_le(src, s, e) return rd_int_basic(src, s, e - 1, 1) end

		-- int rd_int_be(string src, int s, int e)
		-- @src - Source binary string
		-- @s - Start index of a big endian integer
		-- @e - End index of the integer
		local function rd_int_be(src, s, e) return rd_int_basic(src, e - 1, s, -1) end

		-- float rd_flt_le(string src, int s)
		-- @src - Source binary string
		-- @s - Start index of little endian float
		local function rd_flt_le(src, s) return rd_flt_basic(string.byte(src, s, s + 3)) end

		-- float rd_flt_be(string src, int s)
		-- @src - Source binary string
		-- @s - Start index of big endian float
		local function rd_flt_be(src, s)
			local f1, f2, f3, f4 = string.byte(src, s, s + 3)
			return rd_flt_basic(f4, f3, f2, f1)
		end

		-- double rd_dbl_le(string src, int s)
		-- @src - Source binary string
		-- @s - Start index of little endian double
		local function rd_dbl_le(src, s) return rd_dbl_basic(string.byte(src, s, s + 7)) end

		-- double rd_dbl_be(string src, int s)
		-- @src - Source binary string
		-- @s - Start index of big endian double
		local function rd_dbl_be(src, s)
			local f1, f2, f3, f4, f5, f6, f7, f8 = string.byte(src, s, s + 7) -- same
			return rd_dbl_basic(f8, f7, f6, f5, f4, f3, f2, f1)
		end

		-- to avoid nested ifs in deserializing
		local float_types = {
			[4] = {little = rd_flt_le, big = rd_flt_be},
			[8] = {little = rd_dbl_le, big = rd_dbl_be},
		}

		-- byte stm_byte(Stream S)
		-- @S - Stream object to read from
		local function stm_byte(S)
			local idx = S.index
			local bt = string.byte(S.source, idx, idx)

			S.index = idx + 1
			return bt
		end

		-- string stm_string(Stream S, int len)
		-- @S - Stream object to read from
		-- @len - Length of string being read
		local function stm_string(S, len)
			local pos = S.index + len
			local str = string.sub(S.source, S.index, pos - 1)

			S.index = pos
			return str
		end

		-- string stm_lstring(Stream S)
		-- @S - Stream object to read from
		local function stm_lstring(S)
			local len = S:s_szt()
			local str

			if len ~= 0 then str = string.sub(stm_string(S, len), 1, -2) end

			return str
		end

		-- fn cst_int_rdr(string src, int len, fn func)
		-- @len - Length of type for reader
		-- @func - Reader callback
		local function cst_int_rdr(len, func)
			return function(S)
				local pos = S.index + len
				local int = func(S.source, S.index, pos)
				S.index = pos

				return int
			end
		end

		-- fn cst_flt_rdr(string src, int len, fn func)
		-- @len - Length of type for reader
		-- @func - Reader callback
		local function cst_flt_rdr(len, func)
			return function(S)
				local flt = func(S.source, S.index)
				S.index = S.index + len

				return flt
			end
		end

		local function stm_inst_list(S)
			local len = S:s_int()
			local list = table.create(len)

			for i = 1, len do
				local ins = S:s_ins()
				local op = bit.band(ins, 0x3F)
				local args = OPCODE_T[op]
				local mode = OPCODE_M[op]
				local data = {value = ins, op = OPCODE_RM[op], A = bit.band(bit.rshift(ins, 6), 0xFF)}

				if args == 'ABC' then
					data.B = bit.band(bit.rshift(ins, 23), 0x1FF)
					data.C = bit.band(bit.rshift(ins, 14), 0x1FF)
					data.is_KB = mode.b == 'OpArgK' and data.B > 0xFF -- post process optimization
					data.is_KC = mode.c == 'OpArgK' and data.C > 0xFF
				elseif args == 'ABx' then
					data.Bx = bit.band(bit.rshift(ins, 14), 0x3FFFF)
					data.is_K = mode.b == 'OpArgK'
				elseif args == 'AsBx' then
					data.sBx = bit.band(bit.rshift(ins, 14), 0x3FFFF) - 131071
				end

				list[i] = data
			end

			return list
		end

		local function stm_const_list(S)
			local len = S:s_int()
			local list = table.create(len)

			for i = 1, len do
				local tt = stm_byte(S)
				local k

				if tt == 1 then
					k = stm_byte(S) ~= 0
				elseif tt == 3 then
					k = S:s_num()
				elseif tt == 4 then
					k = stm_lstring(S)
				end

				list[i] = k -- offset +1 during instruction decode
			end

			return list
		end

		local function stm_sub_list(S, src)
			local len = S:s_int()
			local list = table.create(len)

			for i = 1, len do
				list[i] = stm_lua_func(S, src) -- offset +1 in CLOSURE
			end

			return list
		end

		local function stm_line_list(S)
			local len = S:s_int()
			local list = table.create(len)

			for i = 1, len do list[i] = S:s_int() end

			return list
		end

		local function stm_loc_list(S)
			local len = S:s_int()
			local list = table.create(len)

			for i = 1, len do list[i] = {varname = stm_lstring(S), startpc = S:s_int(), endpc = S:s_int()} end

			return list
		end

		local function stm_upval_list(S)
			local len = S:s_int()
			local list = table.create(len)

			for i = 1, len do list[i] = stm_lstring(S) end

			return list
		end

		function stm_lua_func(S, psrc)
			local proto = {}
			local src = stm_lstring(S) or psrc -- source is propagated

			proto.source = src -- source name

			S:s_int() -- line defined
			S:s_int() -- last line defined

			proto.num_upval = stm_byte(S) -- num upvalues
			proto.num_param = stm_byte(S) -- num params

			stm_byte(S) -- vararg flag
			proto.max_stack = stm_byte(S) -- max stack size

			proto.code = stm_inst_list(S)
			proto.const = stm_const_list(S)
			proto.subs = stm_sub_list(S, src)
			proto.lines = stm_line_list(S)

			stm_loc_list(S)
			stm_upval_list(S)

			-- post process optimization
			for _, v in ipairs(proto.code) do
				if v.is_K then
					v.const = proto.const[v.Bx + 1] -- offset for 1 based index
				else
					if v.is_KB then v.const_B = proto.const[v.B - 0xFF] end

					if v.is_KC then v.const_C = proto.const[v.C - 0xFF] end
				end
			end

			return proto
		end

		function lua_bc_to_state(src)
			-- func reader
			local rdr_func

			-- header flags
			local little
			local size_int
			local size_szt
			local size_ins
			local size_num
			local flag_int

			-- stream object
			local stream = {
				-- data
				index = 1,
				source = src,
			}

			assert(stm_string(stream, 4) == '\27Lua', 'invalid Lua signature')
			assert(stm_byte(stream) == 0x51, 'invalid Lua version')
			assert(stm_byte(stream) == 0, 'invalid Lua format')

			little = stm_byte(stream) ~= 0
			size_int = stm_byte(stream)
			size_szt = stm_byte(stream)
			size_ins = stm_byte(stream)
			size_num = stm_byte(stream)
			flag_int = stm_byte(stream) ~= 0

			rdr_func = little and rd_int_le or rd_int_be
			stream.s_int = cst_int_rdr(size_int, rdr_func)
			stream.s_szt = cst_int_rdr(size_szt, rdr_func)
			stream.s_ins = cst_int_rdr(size_ins, rdr_func)

			if flag_int then
				stream.s_num = cst_int_rdr(size_num, rdr_func)
			elseif float_types[size_num] then
				stream.s_num = cst_flt_rdr(size_num, float_types[size_num][little and 'little' or 'big'])
			else
				error('unsupported float size')
			end

			return stm_lua_func(stream, '@virtual')
		end

		local function close_lua_upvalues(list, index)
			for i, uv in pairs(list) do
				if uv.index >= index then
					uv.value = uv.store[uv.index] -- store value
					uv.store = uv
					uv.index = 'value' -- self reference
					list[i] = nil
				end
			end
		end

		local function open_lua_upvalue(list, index, memory)
			local prev = list[index]

			if not prev then
				prev = {index = index, store = memory}
				list[index] = prev
			end

			return prev
		end

		local function on_lua_error(failed, err)
			local line = failed.lines[failed.pc - 1]

			error('Line :'.. line.. ": ".. err, 0)
		end

		local function run_lua_func(state, env, upvals)
			local code = state.code
			local subs = state.subs
			local vararg = state.vararg

			local top_index = -1
			local open_list = {}
			local memory = state.memory
			local pc = state.pc

			while true do
				local inst = code[pc]
				local op = inst.op
				pc = pc + 1

				if op < 18 then
					if op < 8 then
						if op < 3 then
							if op < 1 then
								--[[LOADNIL]]
								for i = inst.A, inst.B do memory[i] = nil end
							elseif op > 1 then
								--[[GETUPVAL]]
								local uv = upvals[inst.B]

								memory[inst.A] = uv.store[uv.index]
							else
								--[[ADD]]
								local lhs, rhs

								if inst.is_KB then
									lhs = inst.const_B
								else
									lhs = memory[inst.B]
								end

								if inst.is_KC then
									rhs = inst.const_C
								else
									rhs = memory[inst.C]
								end

								memory[inst.A] = lhs + rhs
							end
						elseif op > 3 then
							if op < 6 then
								if op > 4 then
									--[[SELF]]
									local A = inst.A
									local B = inst.B
									local index

									if inst.is_KC then
										index = inst.const_C
									else
										index = memory[inst.C]
									end

									memory[A + 1] = memory[B]
									memory[A] = memory[B][index]
								else
									--[[GETGLOBAL]]
									memory[inst.A] = env[inst.const]
								end
							elseif op > 6 then
								--[[GETTABLE]]
								local index

								if inst.is_KC then
									index = inst.const_C
								else
									index = memory[inst.C]
								end

								memory[inst.A] = memory[inst.B][index]
							else
								--[[SUB]]
								local lhs, rhs

								if inst.is_KB then
									lhs = inst.const_B
								else
									lhs = memory[inst.B]
								end

								if inst.is_KC then
									rhs = inst.const_C
								else
									rhs = memory[inst.C]
								end

								memory[inst.A] = lhs - rhs
							end
						else --[[MOVE]]
							memory[inst.A] = memory[inst.B]
						end
					elseif op > 8 then
						if op < 13 then
							if op < 10 then
								--[[SETGLOBAL]]
								env[inst.const] = memory[inst.A]
							elseif op > 10 then
								if op < 12 then
									--[[CALL]]
									local A = inst.A
									local B = inst.B
									local C = inst.C
									local params

									if B == 0 then
										params = top_index - A
									else
										params = B - 1
									end

									local ret_list = table.pack(memory[A](table.unpack(memory, A + 1, A + params)))
									local ret_num = ret_list.n

									if C == 0 then
										top_index = A + ret_num - 1
									else
										ret_num = C - 1
									end

									table.move(ret_list, 1, ret_num, A, memory)
								else
									--[[SETUPVAL]]
									local uv = upvals[inst.B]

									uv.store[uv.index] = memory[inst.A]
								end
							else
								--[[MUL]]
								local lhs, rhs

								if inst.is_KB then
									lhs = inst.const_B
								else
									lhs = memory[inst.B]
								end

								if inst.is_KC then
									rhs = inst.const_C
								else
									rhs = memory[inst.C]
								end

								memory[inst.A] = lhs * rhs
							end
						elseif op > 13 then
							if op < 16 then
								if op > 14 then
									--[[TAILCALL]]
									local A = inst.A
									local B = inst.B
									local params

									if B == 0 then
										params = top_index - A
									else
										params = B - 1
									end

									close_lua_upvalues(open_list, 0)

									return memory[A](table.unpack(memory, A + 1, A + params))
								else
									--[[SETTABLE]]
									local index, value

									if inst.is_KB then
										index = inst.const_B
									else
										index = memory[inst.B]
									end

									if inst.is_KC then
										value = inst.const_C
									else
										value = memory[inst.C]
									end

									memory[inst.A][index] = value
								end
							elseif op > 16 then
								--[[NEWTABLE]]
								memory[inst.A] = {}
							else
								--[[DIV]]
								local lhs, rhs

								if inst.is_KB then
									lhs = inst.const_B
								else
									lhs = memory[inst.B]
								end

								if inst.is_KC then
									rhs = inst.const_C
								else
									rhs = memory[inst.C]
								end

								memory[inst.A] = lhs / rhs
							end
						else
							--[[LOADK]]
							memory[inst.A] = inst.const
						end
					else
						--[[FORLOOP]]
						local A = inst.A
						local step = memory[A + 2]
						local index = memory[A] + step
						local limit = memory[A + 1]
						local loops

						if step == math.abs(step) then
							loops = index <= limit
						else
							loops = index >= limit
						end

						if loops then
							memory[A] = index
							memory[A + 3] = index
							pc = pc + inst.sBx
						end
					end
				elseif op > 18 then
					if op < 28 then
						if op < 23 then
							if op < 20 then
								--[[LEN]]
								memory[inst.A] = #memory[inst.B]
							elseif op > 20 then
								if op < 22 then
									--[[RETURN]]
									local A = inst.A
									local B = inst.B
									local len

									if B == 0 then
										len = top_index - A + 1
									else
										len = B - 1
									end

									close_lua_upvalues(open_list, 0)

									return table.unpack(memory, A, A + len - 1)
								else
									--[[CONCAT]]
									local B = inst.B
									local str = memory[B]

									for i = B + 1, inst.C do str = str .. memory[i] end

									memory[inst.A] = str
								end
							else
								--[[MOD]]
								local lhs, rhs

								if inst.is_KB then
									lhs = inst.const_B
								else
									lhs = memory[inst.B]
								end

								if inst.is_KC then
									rhs = inst.const_C
								else
									rhs = memory[inst.C]
								end

								memory[inst.A] = lhs % rhs
							end
						elseif op > 23 then
							if op < 26 then
								if op > 24 then
									--[[CLOSE]]
									close_lua_upvalues(open_list, inst.A)
								else
									--[[EQ]]
									local lhs, rhs

									if inst.is_KB then
										lhs = inst.const_B
									else
										lhs = memory[inst.B]
									end

									if inst.is_KC then
										rhs = inst.const_C
									else
										rhs = memory[inst.C]
									end

									if (lhs == rhs) == (inst.A ~= 0) then pc = pc + code[pc].sBx end

									pc = pc + 1
								end
							elseif op > 26 then
								--[[LT]]
								local lhs, rhs

								if inst.is_KB then
									lhs = inst.const_B
								else
									lhs = memory[inst.B]
								end

								if inst.is_KC then
									rhs = inst.const_C
								else
									rhs = memory[inst.C]
								end

								if (lhs < rhs) == (inst.A ~= 0) then pc = pc + code[pc].sBx end

								pc = pc + 1
							else
								--[[POW]]
								local lhs, rhs

								if inst.is_KB then
									lhs = inst.const_B
								else
									lhs = memory[inst.B]
								end

								if inst.is_KC then
									rhs = inst.const_C
								else
									rhs = memory[inst.C]
								end

								memory[inst.A] = lhs ^ rhs
							end
						else
							--[[LOADBOOL]]
							memory[inst.A] = inst.B ~= 0

							if inst.C ~= 0 then pc = pc + 1 end
						end
					elseif op > 28 then
						if op < 33 then
							if op < 30 then
								--[[LE]]
								local lhs, rhs

								if inst.is_KB then
									lhs = inst.const_B
								else
									lhs = memory[inst.B]
								end

								if inst.is_KC then
									rhs = inst.const_C
								else
									rhs = memory[inst.C]
								end

								if (lhs <= rhs) == (inst.A ~= 0) then pc = pc + code[pc].sBx end

								pc = pc + 1
							elseif op > 30 then
								if op < 32 then
									--[[CLOSURE]]
									local sub = subs[inst.Bx + 1] -- offset for 1 based index
									local nups = sub.num_upval
									local uvlist

									if nups ~= 0 then
										uvlist = {}

										for i = 1, nups do
											local pseudo = code[pc + i - 1]

											if pseudo.op == OPCODE_RM[0] then -- @MOVE
												uvlist[i - 1] = open_lua_upvalue(open_list, pseudo.B, memory)
											elseif pseudo.op == OPCODE_RM[4] then -- @GETUPVAL
												uvlist[i - 1] = upvals[pseudo.B]
											end
										end

										pc = pc + nups
									end

									memory[inst.A] = lua_wrap_state(sub, env, uvlist)
								else
									--[[TESTSET]]
									local A = inst.A
									local B = inst.B

									if (not memory[B]) ~= (inst.C ~= 0) then
										memory[A] = memory[B]
										pc = pc + code[pc].sBx
									end
									pc = pc + 1
								end
							else
								--[[UNM]]
								memory[inst.A] = -memory[inst.B]
							end
						elseif op > 33 then
							if op < 36 then
								if op > 34 then
									--[[VARARG]]
									local A = inst.A
									local len = inst.B

									if len == 0 then
										len = vararg.len
										top_index = A + len - 1
									end

									table.move(vararg.list, 1, len, A, memory)
								else
									--[[FORPREP]]
									local A = inst.A
									local init, limit, step

									init = assert(tonumber(memory[A]), '`for` initial value must be a number')
									limit = assert(tonumber(memory[A + 1]), '`for` limit must be a number')
									step = assert(tonumber(memory[A + 2]), '`for` step must be a number')

									memory[A] = init - step
									memory[A + 1] = limit
									memory[A + 2] = step

									pc = pc + inst.sBx
								end
							elseif op > 36 then
								--[[SETLIST]]
								local A = inst.A
								local C = inst.C
								local len = inst.B
								local tab = memory[A]
								local offset

								if len == 0 then len = top_index - A end

								if C == 0 then
									C = inst[pc].value
									pc = pc + 1
								end

								offset = (C - 1) * FIELDS_PER_FLUSH

								table.move(memory, A + 1, A + len, offset + 1, tab)
							else
								--[[NOT]]
								memory[inst.A] = not memory[inst.B]
							end
						else
							--[[TEST]]
							if (not memory[inst.A]) ~= (inst.C ~= 0) then pc = pc + code[pc].sBx end
							pc = pc + 1
						end
					else
						--[[TFORLOOP]]
						local A = inst.A
						local base = A + 3

						local vals = {memory[A](memory[A + 1], memory[A + 2])}

						table.move(vals, 1, inst.C, base, memory)

						if memory[base] ~= nil then
							memory[A + 2] = memory[base]
							pc = pc + code[pc].sBx
						end

						pc = pc + 1
					end
				else
					--[[JMP]]
					pc = pc + inst.sBx
				end

				state.pc = pc
			end
		end

		function lua_wrap_state(proto, env, upval)
			local function wrapped(...)
				local passed = table.pack(...)
				local memory = table.create(proto.max_stack)
				local vararg = {len = 0, list = {}}

				table.move(passed, 1, proto.num_param, 0, memory)

				if proto.num_param < passed.n then
					local start = proto.num_param + 1
					local len = passed.n - proto.num_param

					vararg.len = len
					table.move(passed, start, start + len - 1, 1, vararg.list)
				end

				local state = {vararg = vararg, memory = memory, code = proto.code, subs = proto.subs, pc = 1}

				local result = table.pack(pcall(run_lua_func, state, env, upval))

				if result[1] then
					return table.unpack(result, 2, result.n)
				else
					local failed = {pc = state.pc, source = proto.source, lines = proto.lines}

					on_lua_error(failed, result[2])

					return
				end
			end

			return wrapped
		end

		return function(bCode, env)
			return lua_wrap_state(lua_bc_to_state(bCode), env or getfenv(0))
		end
	end)()
	--getfenv().script = nil

	return function(source, env)
		local executable
		local env = env or getfenv(2)
		local ran, failureReason = pcall(function()
			local compiledBytecode = compile(source,  "shg340934qh")
			executable = createExecutable(compiledBytecode, env)
		end)

		if ran then
			return setfenv(executable, env)
		end
		return nil, failureReason
	end
end)()

local utils = {}

utils.string_to_hex = function(value, offset, seperator)
	offset = offset or 227
	seperator = seperator or ""

	return string.gsub(value, ".", function(char)
		return string.format("%02X", string.byte(char) * offset) .. seperator
	end)
end

utils.hex_to_string = function(value, offset)
	offset = offset or 1

	return string.gsub(value, "..", function(char)
		return tonumber(char, 16):: number / 1
	end)
end

local INTgetloadedmodules = function()
	local tab = table.create(0)
	for i, v in pairs(game:GetDescendants()) do
		if v.ClassName == "ModuleScript" then
			table.insert(tab,v)
		end
	end
	return tab
end

utils.fetch_modules = INTgetloadedmodules

utils.base64_encode = function(data)
	local letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
	return ((data:gsub('.', function(x) 
		local r,b='',x:byte()
		for i=8,1,-1 do r=r..(b%2^i-b%2^(i-1)>0 and '1' or '0') end
		return r;
	end)..'0000'):gsub('%d%d%d?%d?%d?%d?', function(x)
		if (#x < 6) then return '' end
		local c=0
		for i=1,6 do c=c+(x:sub(i,i)=='1' and 2^(6-i) or 0) end
		return letters:sub(c+1,c+1)
	end)..({ '', '==', '=' })[#data%3+1])
end
utils.base64_decode = function(data)
	local b = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
	data = string.gsub(data, '[^'..b..'=]', '')
	return (data:gsub('.', function(x)
		if x == '=' then return '' end
		local r, f = '', (b:find(x) - 1)
		for i = 6, 1, -1 do
			r = r .. (f % 2^i - f % 2^(i - 1) > 0 and '1' or '0')
		end
		return r;
	end):gsub('%d%d%d?%d?%d?%d?%d?%d?', function(x)
		if #x ~= 8 then return '' end
		local c = 0
		for i = 1, 8 do
			c = c + (x:sub(i, i) == '1' and 2^(8 - i) or 0)
		end
		return string.char(c)
	end))
end

local lz4 = {}

type Streamer = {
	Offset: number,
	Source: string,
	Length: number,
	IsFinished: boolean,
	LastUnreadBytes: number,

	read: (Streamer, len: number?, shiftOffset: boolean?) -> string,
	seek: (Streamer, len: number) -> (),
	append: (Streamer, newData: string) -> (),
	toEnd: (Streamer) -> ()
}

type BlockData = {
	[number]: {
		Literal: string,
		LiteralLength: number,
		MatchOffset: number?,
		MatchLength: number?
	}
}

local function plainFind(str, pat)
	return string.find(str, pat, 0, true)
end

local function streamer(str): Streamer
	local Stream = {}
	Stream.Offset = 0
	Stream.Source = str
	Stream.Length = string.len(str)
	Stream.IsFinished = false	
	Stream.LastUnreadBytes = 0

	function Stream.read(self: Streamer, len: number?, shift: boolean?): string
		local len = len or 1
		local shift = if shift ~= nil then shift else true
		local dat = string.sub(self.Source, self.Offset + 1, self.Offset + len)

		local dataLength = string.len(dat)
		local unreadBytes = len - dataLength

		if shift then
			self:seek(len)
		end

		self.LastUnreadBytes = unreadBytes
		return dat
	end

	function Stream.seek(self: Streamer, len: number)
		local len = len or 1

		self.Offset = math.clamp(self.Offset + len, 0, self.Length)
		self.IsFinished = self.Offset >= self.Length
	end

	function Stream.append(self: Streamer, newData: string)
		-- adds new data to the end of a stream
		self.Source ..= newData
		self.Length = string.len(self.Source)
		self:seek(0) --hacky but forces a recalculation of the isFinished flag
	end

	function Stream.toEnd(self: Streamer)
		self:seek(self.Length)
	end

	return Stream
end

function lz4.compress(str: string): string
	local blocks: BlockData = {}
	local iostream = streamer(str)

	if iostream.Length > 12 then
		local firstFour = iostream:read(4)

		local processed = firstFour
		local lit = firstFour
		local match = ""
		local LiteralPushValue = ""
		local pushToLiteral = true

		repeat
			pushToLiteral = true
			local nextByte = iostream:read()

			if plainFind(processed, nextByte) then
				local next3 = iostream:read(3, false)

				if string.len(next3) < 3 then
					--push bytes to literal block then break
					LiteralPushValue = nextByte .. next3
					iostream:seek(3)
				else
					match = nextByte .. next3

					local matchPos = plainFind(processed, match)
					if matchPos then
						iostream:seek(3)
						repeat
							local nextMatchByte = iostream:read(1, false)
							local newResult = match .. nextMatchByte

							local repos = plainFind(processed, newResult) 
							if repos then
								match = newResult
								matchPos = repos
								iostream:seek(1)
							end
						until not plainFind(processed, newResult) or iostream.IsFinished

						local matchLen = string.len(match)
						local pushMatch = true

						if iostream.Length - iostream.Offset <= 5 then
							LiteralPushValue = match
							pushMatch = false
							--better safe here, dont bother pushing to match ever
						end

						if pushMatch then
							pushToLiteral = false

							-- gets the position from the end of processed, then slaps it onto processed
							local realPosition = string.len(processed) - matchPos
							processed = processed .. match

							table.insert(blocks, {
								Literal = lit,
								LiteralLength = string.len(lit),
								MatchOffset = realPosition + 1,
								MatchLength = matchLen,
							})
							lit = ""
						end
					else
						LiteralPushValue = nextByte
					end
				end
			else
				LiteralPushValue = nextByte
			end

			if pushToLiteral then
				lit = lit .. LiteralPushValue
				processed = processed .. nextByte
			end
		until iostream.IsFinished
		table.insert(blocks, {
			Literal = lit,
			LiteralLength = string.len(lit)
		})
	else
		local str = iostream.Source
		blocks[1] = {
			Literal = str,
			LiteralLength = string.len(str)
		}
	end

	-- generate the output chunk
	-- %s is for adding header
	local output = string.rep("\x00", 4)
	local function write(char)
		output = output .. char
	end
	-- begin working through chunks
	for chunkNum, chunk in blocks do
		local litLen = chunk.LiteralLength
		local matLen = (chunk.MatchLength or 4) - 4

		-- create token
		local tokenLit = math.clamp(litLen, 0, 15)
		local tokenMat = math.clamp(matLen, 0, 15)

		local token = bit32.lshift(tokenLit, 4) + tokenMat
		write(string.pack("<I1", token))

		if litLen >= 15 then
			litLen = litLen - 15
			--begin packing extra bytes
			repeat
				local nextToken = math.clamp(litLen, 0, 0xFF)
				write(string.pack("<I1", nextToken))
				if nextToken == 0xFF then
					litLen = litLen - 255
				end
			until nextToken < 0xFF
		end

		-- push raw lit data
		write(chunk.Literal)

		if chunkNum ~= #blocks then
			-- push offset as u16
			write(string.pack("<I2", chunk.MatchOffset))

			-- pack extra match bytes
			if matLen >= 15 then
				matLen = matLen - 15

				repeat
					local nextToken = math.clamp(matLen, 0, 0xFF)
					write(string.pack("<I1", nextToken))
					if nextToken == 0xFF then
						matLen = matLen - 255
					end
				until nextToken < 0xFF
			end
		end
	end
	--append chunks
	local compLen = string.len(output) - 4
	local decompLen = iostream.Length

	return string.pack("<I4", compLen) .. string.pack("<I4", decompLen) .. output
end

function lz4.decompress(lz4data: string): string
	local inputStream = streamer(lz4data)

	local compressedLen = string.unpack("<I4", inputStream:read(4))
	local decompressedLen = string.unpack("<I4", inputStream:read(4))
	local reserved = string.unpack("<I4", inputStream:read(4))

	if compressedLen == 0 then
		return inputStream:read(decompressedLen)
	end

	local outputStream = streamer("")

	repeat
		local token = string.byte(inputStream:read())
		local litLen = bit32.rshift(token, 4)
		local matLen = bit32.band(token, 15) + 4

		if litLen >= 15 then
			repeat
				local nextByte = string.byte(inputStream:read())
				litLen += nextByte
			until nextByte ~= 0xFF
		end

		local literal = inputStream:read(litLen)
		outputStream:append(literal)
		outputStream:toEnd()
		if outputStream.Length < decompressedLen then
			--match
			local offset = string.unpack("<I2", inputStream:read(2))
			if matLen >= 19 then
				repeat
					local nextByte = string.byte(inputStream:read())
					matLen += nextByte
				until nextByte ~= 0xFF
			end

			outputStream:seek(-offset)
			local pos = outputStream.Offset
			local match = outputStream:read(matLen)
			local unreadBytes = outputStream.LastUnreadBytes
			local extra
			if unreadBytes then
				repeat
					outputStream.Offset = pos
					extra = outputStream:read(unreadBytes)
					unreadBytes = outputStream.LastUnreadBytes
					match ..= extra
				until unreadBytes <= 0
			end

			outputStream:append(match)
			outputStream:toEnd()
		end

	until outputStream.Length >= decompressedLen

	return outputStream.Source
end


-- BRIDGE

local bridge = {}

bridge.validActions = {
	--script stuff
	["load_source"] = {"string", "string"},
	["HttpGet"] = {},
	["get_script"] = {},
	--file system lib
	["writefile"] = {}, 
	["isfile"] = {},
	["isfile"] = {},
	["delfile"] = {},
	["readfile"] = {},
	["makefolder"] = {},
	["delfolder"] = {},
	["isfolder"] = {},
	--misc
	["gethwid"] = {},
	["getbytecode"] = {},
	["input_action"] = {},
	["messagebox"] = {},
	["loadautoexec"] = {},
	--clipboard
	["getclipboard"] = {},
	["setclipboard"] = {},
	--console 
	["rconsolecreate"] = {},	
} -- go to loadstring to change it



function bridge:send(action, ...) 
	local args = {...}
	local success, res = pcall(function()

		if not self.validActions[action] then
			warn("[ DEBUG ] Bridge.send -> cancelled")
			return
		end

		local url = "http://localhost:8000/bridge?action=" .. action
		for i, arg in ipairs(args) do
			url = url .. "&arg" .. i .. "=" .. HttpService:UrlEncode(tostring(arg))
		end
		local params = {
			Url = url,
			Method = "GET",
			Headers = {
				["Content-Type"] = "application/json"
			}
		}
		local request = HttpService:RequestInternal(params)
		local response = nil
		local requestCompletedEvent = Instance.new("BindableEvent")
		request:Start(function(success, result)
			response = result
			requestCompletedEvent:Fire()
		end)
		requestCompletedEvent.Event:Wait()

		if response.StatusMessage == "OK" then 
			return HttpService:JSONDecode(response.Body) or HttpService:JSONDecode(web_response.Body)
		end
	end)
	if not success then
		warn("[ ERROR ] -> "..tostring(res))
	else
		return res
	end
end

local old = load

local val_the_sigma = function(code)
	local module = utils.fetch_modules()[1]:Clone()
	module.Source = "return function(...) " .. code .. " end"
	local func = require(module)
	func = setfenv(func, getfenv())
	return func
end

-- ENVIROMENT

local function get_buildins()
	return {
		-- Luau Functions
		["assert"] = "function",
		["error"] = "function",
		["getfenv"] = "function",
		["getmetatable"] = "function",
		["ipairs"] = "function",
		["loadstring"] = "function",
		["newproxy"] = "function",
		["next"] = "function",
		["pairs"] = "function",
		["pcall"] = "function",
		["print"] = "function",
		["rawequal"] = "function",
		["rawget"] = "function",
		["rawlen"] = "function",
		["rawset"] = "function",
		["select"] = "function",
		["setfenv"] = "function",
		["setmetatable"] = "function",
		["tonumber"] = "function",
		["tostring"] = "function",
		["unpack"] = "function",
		["xpcall"] = "function",
		["type"] = "function",
		["typeof"] = "function",

		-- Luau Functions (Deprecated)
		["collectgarbage"] = "function",

		-- Luau Variables
		["_G"] = "table",
		["_VERSION"] = "string",

		-- Luau Tables
		["bit32"] = "table",
		["coroutine"] = "table",
		["debug"] = "table",
		["math"] = "table",
		["os"] = "table",
		["string"] = "table",
		["table"] = "table",
		["utf8"] = "table",
		["buffer"] = "table",

		-- Roblox Functions
		["DebuggerManager"] = "function",
		["delay"] = "function",
		["gcinfo"] = "function",
		["PluginManager"] = "function",
		["require"] = "function",
		["settings"] = "function",
		["spawn"] = "function",
		["tick"] = "function",
		["time"] = "function",
		["UserSettings"] = "function",
		["wait"] = "function",
		["warn"] = "function",

		-- Roblox Functions (Deprecated)
		["Delay"] = "function",
		["ElapsedTime"] = "function",
		["elapsedTime"] = "function",
		["printidentity"] = "function",
		["Spawn"] = "function",
		["Stats"] = "function",
		["stats"] = "function",
		["Version"] = "function",
		["version"] = "function",
		["Wait"] = "function",
		["ypcall"] = "function",

		-- Roblox Variables
		["game"] = "Instance",
		["plugin"] = "Instance",
		["shared"] = "table",
		["workspace"] = "Instance",

		-- Roblox Variables (Deprecated)
		["Game"] = "Instance",
		["Workspace"] = "Instance",

		-- Roblox Tables
		["Axes"] = "table",
		["BrickColor"] = "table",
		["CatalogSearchParams"] = "table",
		["CFrame"] = "table",
		["Color3"] = "table",
		["ColorSequence"] = "table",
		["ColorSequenceKeypoint"] = "table",
		["DateTime"] = "table",
		["DockWidgetPluginGuiInfo"] = "table",
		["Enum"] = "Enums",
		["Faces"] = "table",
		["FloatCurveKey"] = "table",
		["Font"] = "table",
		["Instance"] = "table",
		["NumberRange"] = "table",
		["NumberSequence"] = "table",
		["NumberSequenceKeypoint"] = "table",
		["OverlapParams"] = "table",
		["PathWaypoint"] = "table",
		["PhysicalProperties"] = "table",
		["Random"] = "table",
		["Ray"] = "table",
		["RaycastParams"] = "table",
		["Rect"] = "table",
		["Region3"] = "table",
		["Region3int16"] = "table",
		["RotationCurveKey"] = "table",
		["SharedTable"] = "table",
		["task"] = "table",
		["TweenInfo"] = "table",
		["UDim"] = "table",
		["UDim2"] = "table",
		["Vector2"] = "table",
		["Vector2int16"] = "table",
		["Vector3"] = "table",
		["Vector3int16"] = "table",
	}
end

local function _apply_env(func, env)
	setfenv(0, env)
	setfenv(1, env)

	return setfenv(func, env)
end

local sandbox = {
	environment = nil,
	hidden_env = nil,
	blacklisted_extensions = {
		".exe", ".bat", ".com", ".cmd", ".inf", ".ipa",
		".apk", ".apkm", ".osx", ".pif", ".run", ".wsh",
		".bin", ".app", ".vb", ".vbs", ".scr", ".fap",
		".cpl", ".inf1", ".ins", ".inx", ".isu", ".job",
		".lnk", ".msi", ".ps1", ".reg", ".vbe", ".js",
		".x86", ".xlm", ".scpt", ".out", ".ba_", ".jar",
		".ahk", ".xbe", ".0xe", ".u3p", ".bms", ".jse",
		".ex", ".rar", ".zip", ".7z", ".py", ".cpp",
		".cs", ".prx", ".tar", ".", ".wim", ".htm",
		".html", ".css", ".appimage", ".applescript",
		".x86_64", ".x64_64", ".autorun", ".tmp", ".sys",
		".dat", ".ini", ".pol", ".vbscript", ".gadget",
		".workflow", ".script", ".action", ".command",
		".arscript", ".psc1",
	}
}

local main_script = script
local setfenv = setfenv

function sandbox:apply<T>(func: T): T
	local new_env = self:new_environment(Instance.new("LocalScript"))
	local _success, result = coroutine.resume(coroutine.create(_apply_env), func, new_env)

	return result
end

function sandbox:new_environment(script: LuaSourceContainer): { [any]: any }
	local plugin_env = {}
	local sandboxed_env = setmetatable({
		script = script,
	}, {
		__index = function(env, index)
			return plugin_env[index] or self.hidden_env[index] or self.environment.global[index]
		end,
		__newindex = function(env, index, value)
			if index ~= "script" then
				rawset(self.environment.global, index, value)
			end
			rawset(env, index, value)
		end,
	})

	plugin_env.getfenv = function(value)
		local success, result = pcall(getfenv, value)

		if success then
			if rawget(result, "script") == main_script then
				result = sandboxed_env
			end
			return result
		end
		return error(result, 2)
	end

	return sandboxed_env
end

local function initialize_env_modules()
	local roblox_env = get_buildins() -- do not write on this
	local new_env = setmetatable({}, {
		__index = roblox_env,
	})

	-- init
	for index, value in new_env do
		if type(value) ~= "table" or table.isfrozen(value) then
			continue
		end
		table.freeze(value)
	end

	return {
		global = new_env,
		roblox = roblox_env,
	}
end

function sandbox:initialize()
	local hidden_env = {
		game = newproxy(true),
	}
	local environment = initialize_env_modules()

	local _game_meta = getmetatable(hidden_env.game)
	_game_meta.__index = function(self, index)
		local _, game_index = pcall(function()
			return game[index]
		end)

		if game_index and type(game_index) == "function" then
			return function(self, ...)
				return game_index(game, ...)
			end
		else
			--TODO Add automatic .GetService
			return game[index]
		end
	end
	_game_meta.__metatable = getmetatable(game)

	local cloned_environment

	environment.global._G = {}
	environment.global.shared = {}
	environment.global.crypt = {
		base64 = {},
		hex = {}, 
		url = {},
	}
	environment.global.bincogver = function()
		return '2.0.0'
	end
	environment.global.cache = {}
	environment.global.http = {}
	environment.global.base64 = {}
	environment.global.debug = table.clone(debug)
	environment.global.syn = {
		crypt = {
			base64 = {},
		},
		crypto = {
			base64 = {},
		},
	}
	environment.global.fluxus = {
		crypt = {},
	}
	function rqst(Options)
		assert(type(Options) == 'table', 'Argument #1 to \'request\' must be a table, got ' .. typeof(Options))
		if typeof(script) == 'Instance' and script.ClassName == 'Script' then
			return HttpService:RequestAsync(Options)
		end
		local Timeout, Done, Time = 5, false, 0
		local Return = {
			Success = false,
			StatusCode = 408,
			StatusMessage = 'Request Timeout',
			Headers = {},
			Body = ''
		}
		local function Callback(Success, Response)
			Done = true
			Return.Success = Success
			Return.StatusCode = Response.StatusCode
			Return.StatusMessage = Response.StatusMessage
			Return.Headers = Response.Headers
			Return.Body = Response.Body
		end
		HttpService:RequestInternal(Options):Start(Callback)
		while not Done and Time < Timeout do -- probably a bad approach?
			Time = Time + .1
			task.wait(.1)
		end
		return Return
	end
	local _game = game
	local Players = _game:GetService("Players")
	environment.global.game = setmetatable({}, {
		__index = function(self, key)
			if key == 'HttpGet' then
			return function(_, Url)
				return rqst({Url = Url, Method = "GET"}).Body
			end
			elseif key == 'HttpGetAsync' then
				return function(_, Url)
					return rqst({Url = Url, Method = "GET"}).Body
				end
			elseif key == 'HttpPost' then
				return function(Url, Data, contentType)
					local Args = {Url = Url, Method = "POST", Body = Data}
					if contentType then
						Args.Headers = {['Content-Type'] = contentType}
					end
					return rqst(Args).Body
				end
			elseif key == 'HttpPostAsync' then
				return function(Url, Data, contentType)
					local Args = {Url = Url, Method = "POST", Body = Data}
					if contentType then
						Args.Headers = {['Content-Type'] = contentType}
					end
					return rqst(Args).Body
				end
			elseif key == 'GetService' then
				local k = _game[key]
				if type(k) == 'function' then
					return function(_, ...)
						local args = {...}
						assert(not table.find(args, "HttpRbxApiService"), "Attempt to call blocked function")
						assert(not table.find(args, "MessageBusService"), "Attempt to call blocked function")
						assert(not table.find(args, "ScriptContext"), "Attempt to call blocked function")
						assert(not table.find(args, "BrowserService"), "Attempt to call blocked function")
						return k(_game, ...)
					end
				else
					return k
				end
			else
					local k = _game[key]
				if type(k) == 'function' then
					return function(_, ...)
						local args = {...}
						return k(_game, ...)
					end
				else
					return k
				end
			end
		end,
		__tostring = function() return tostring(_game) end
	})
	-- services
	local coreGui = game:GetService("CoreGui")
	-- objects
	local camera = game.Workspace.CurrentCamera
	local drawingUI = Instance.new("ScreenGui")
	drawingUI.Name = "Drawing"
	drawingUI.IgnoreGuiInset = true
	drawingUI.DisplayOrder = 0x7fffffff
	drawingUI.Parent = coreGui
	-- variables
	local drawingIndex = 0
	local uiStrokes = table.create(0)
	local baseDrawingObj = setmetatable({
		Visible = true,
		ZIndex = 0,
		Transparency = 1,
		Color = Color3.new(),
		Remove = function(self)
			setmetatable(self, nil)
		end
	}, {
		__add = function(t1, t2)
			local result = table.clone(t1)

			for index, value in t2 do
				result[index] = value
			end
			return result
		end
	})
	local drawingFontsEnum = {
		[0] = Font.fromEnum(Enum.Font.Roboto),
		[1] = Font.fromEnum(Enum.Font.Legacy),
		[2] = Font.fromEnum(Enum.Font.SourceSans),
		[3] = Font.fromEnum(Enum.Font.RobotoMono),
	}
	-- function
	local function getFontFromIndex(fontIndex: number): Font
		return drawingFontsEnum[fontIndex]
	end

	local function convertTransparency(transparency: number): number
		return math.clamp(1 - transparency, 0, 1)
	end
	-- main
	local DrawingLib = {}
	DrawingLib.Fonts = {
		["UI"] = 0,
		["System"] = 1,
		["Plex"] = 2,
		["Monospace"] = 3
	}
	local drawings = {}
	function DrawingLib.new(drawingType)
		drawingIndex += 1
		if drawingType == "Line" then
			local lineObj = ({
				From = Vector2.zero,
				To = Vector2.zero,
				Thickness = 1
			} + baseDrawingObj)

			local lineFrame = Instance.new("Frame")
			lineFrame.Name = drawingIndex
			lineFrame.AnchorPoint = (Vector2.one * .5)
			lineFrame.BorderSizePixel = 0

			lineFrame.BackgroundColor3 = lineObj.Color
			lineFrame.Visible = lineObj.Visible
			lineFrame.ZIndex = lineObj.ZIndex
			lineFrame.BackgroundTransparency = convertTransparency(lineObj.Transparency)

			lineFrame.Size = UDim2.new()

			lineFrame.Parent = drawingUI
			local bs = table.create(0)
			table.insert(drawings,bs)
			return setmetatable(bs, {
				__newindex = function(_, index, value)
					if typeof(lineObj[index]) == "nil" then return end

					if index == "From" then
						local direction = (lineObj.To - value)
						local center = (lineObj.To + value) / 2
						local distance = direction.Magnitude
						local theta = math.deg(math.atan2(direction.Y, direction.X))

						lineFrame.Position = UDim2.fromOffset(center.X, center.Y)
						lineFrame.Rotation = theta
						lineFrame.Size = UDim2.fromOffset(distance, lineObj.Thickness)
					elseif index == "To" then
						local direction = (value - lineObj.From)
						local center = (value + lineObj.From) / 2
						local distance = direction.Magnitude
						local theta = math.deg(math.atan2(direction.Y, direction.X))

						lineFrame.Position = UDim2.fromOffset(center.X, center.Y)
						lineFrame.Rotation = theta
						lineFrame.Size = UDim2.fromOffset(distance, lineObj.Thickness)
					elseif index == "Thickness" then
						local distance = (lineObj.To - lineObj.From).Magnitude

						lineFrame.Size = UDim2.fromOffset(distance, value)
					elseif index == "Visible" then
						lineFrame.Visible = value
					elseif index == "ZIndex" then
						lineFrame.ZIndex = value
					elseif index == "Transparency" then
						lineFrame.BackgroundTransparency = convertTransparency(value)
					elseif index == "Color" then
						lineFrame.BackgroundColor3 = value
					end
					lineObj[index] = value
				end,
				__index = function(self, index)
					if index == "Remove" or index == "Destroy" then
						return function()
							lineFrame:Destroy()
							lineObj.Remove(self)
							return lineObj:Remove()
						end
					end
					return lineObj[index]
				end
			})
		elseif drawingType == "Text" then
			local textObj = ({
				Text = "",
				Font = DrawingLib.Fonts.UI,
				Size = 0,
				Position = Vector2.zero,
				Center = false,
				Outline = false,
				OutlineColor = Color3.new()
			} + baseDrawingObj)

			local textLabel, uiStroke = Instance.new("TextLabel"), Instance.new("UIStroke")
			textLabel.Name = drawingIndex
			textLabel.AnchorPoint = (Vector2.one * .5)
			textLabel.BorderSizePixel = 0
			textLabel.BackgroundTransparency = 1

			textLabel.Visible = textObj.Visible
			textLabel.TextColor3 = textObj.Color
			textLabel.TextTransparency = convertTransparency(textObj.Transparency)
			textLabel.ZIndex = textObj.ZIndex

			textLabel.FontFace = getFontFromIndex(textObj.Font)
			textLabel.TextSize = textObj.Size

			textLabel:GetPropertyChangedSignal("TextBounds"):Connect(function()
				local textBounds = textLabel.TextBounds
				local offset = textBounds / 2

				textLabel.Size = UDim2.fromOffset(textBounds.X, textBounds.Y)
				textLabel.Position = UDim2.fromOffset(textObj.Position.X + (if not textObj.Center then offset.X else 0), textObj.Position.Y + offset.Y)
			end)

			uiStroke.Thickness = 1
			uiStroke.Enabled = textObj.Outline
			uiStroke.Color = textObj.Color

			textLabel.Parent, uiStroke.Parent = drawingUI, textLabel
			local bs = table.create(0)
			table.insert(drawings,bs)
			return setmetatable(bs, {
				__newindex = function(_, index, value)
					if typeof(textObj[index]) == "nil" then return end

					if index == "Text" then
						textLabel.Text = value
					elseif index == "Font" then
						value = math.clamp(value, 0, 3)
						textLabel.FontFace = getFontFromIndex(value)
					elseif index == "Size" then
						textLabel.TextSize = value
					elseif index == "Position" then
						local offset = textLabel.TextBounds / 2

						textLabel.Position = UDim2.fromOffset(value.X + (if not textObj.Center then offset.X else 0), value.Y + offset.Y)
					elseif index == "Center" then
						local position = (
							if value then
								camera.ViewportSize / 2
								else
								textObj.Position
						)

						textLabel.Position = UDim2.fromOffset(position.X, position.Y)
					elseif index == "Outline" then
						uiStroke.Enabled = value
					elseif index == "OutlineColor" then
						uiStroke.Color = value
					elseif index == "Visible" then
						textLabel.Visible = value
					elseif index == "ZIndex" then
						textLabel.ZIndex = value
					elseif index == "Transparency" then
						local transparency = convertTransparency(value)

						textLabel.TextTransparency = transparency
						uiStroke.Transparency = transparency
					elseif index == "Color" then
						textLabel.TextColor3 = value
					end
					textObj[index] = value
				end,
				__index = function(self, index)
					if index == "Remove" or index == "Destroy" then
						return function()
							textLabel:Destroy()
							textObj.Remove(self)
							return textObj:Remove()
						end
					elseif index == "TextBounds" then
						return textLabel.TextBounds
					end
					return textObj[index]
				end
			})
		elseif drawingType == "Circle" then
			local circleObj = ({
				Radius = 150,
				Position = Vector2.zero,
				Thickness = .7,
				Filled = false
			} + baseDrawingObj)

			local circleFrame, uiCorner, uiStroke = Instance.new("Frame"), Instance.new("UICorner"), Instance.new("UIStroke")
			circleFrame.Name = drawingIndex
			circleFrame.AnchorPoint = (Vector2.one * .5)
			circleFrame.BorderSizePixel = 0

			circleFrame.BackgroundTransparency = (if circleObj.Filled then convertTransparency(circleObj.Transparency) else 1)
			circleFrame.BackgroundColor3 = circleObj.Color
			circleFrame.Visible = circleObj.Visible
			circleFrame.ZIndex = circleObj.ZIndex

			uiCorner.CornerRadius = UDim.new(1, 0)
			circleFrame.Size = UDim2.fromOffset(circleObj.Radius, circleObj.Radius)

			uiStroke.Thickness = circleObj.Thickness
			uiStroke.Enabled = not circleObj.Filled
			uiStroke.ApplyStrokeMode = Enum.ApplyStrokeMode.Border

			circleFrame.Parent, uiCorner.Parent, uiStroke.Parent = drawingUI, circleFrame, circleFrame
			local bs = table.create(0)
			table.insert(drawings,bs)
			return setmetatable(bs, {
				__newindex = function(_, index, value)
					if typeof(circleObj[index]) == "nil" then return end

					if index == "Radius" then
						local radius = value * 2
						circleFrame.Size = UDim2.fromOffset(radius, radius)
					elseif index == "Position" then
						circleFrame.Position = UDim2.fromOffset(value.X, value.Y)
					elseif index == "Thickness" then
						value = math.clamp(value, .6, 0x7fffffff)
						uiStroke.Thickness = value
					elseif index == "Filled" then
						circleFrame.BackgroundTransparency = (if value then convertTransparency(circleObj.Transparency) else 1)
						uiStroke.Enabled = not value
					elseif index == "Visible" then
						circleFrame.Visible = value
					elseif index == "ZIndex" then
						circleFrame.ZIndex = value
					elseif index == "Transparency" then
						local transparency = convertTransparency(value)

						circleFrame.BackgroundTransparency = (if circleObj.Filled then transparency else 1)
						uiStroke.Transparency = transparency
					elseif index == "Color" then
						circleFrame.BackgroundColor3 = value
						uiStroke.Color = value
					end
					circleObj[index] = value
				end,
				__index = function(self, index)
					if index == "Remove" or index == "Destroy" then
						return function()
							circleFrame:Destroy()
							circleObj.Remove(self)
							return circleObj:Remove()
						end
					end
					return circleObj[index]
				end
			})
		elseif drawingType == "Square" then
			local squareObj = ({
				Size = Vector2.zero,
				Position = Vector2.zero,
				Thickness = .7,
				Filled = false
			} + baseDrawingObj)

			local squareFrame, uiStroke = Instance.new("Frame"), Instance.new("UIStroke")
			squareFrame.Name = drawingIndex
			squareFrame.BorderSizePixel = 0

			squareFrame.BackgroundTransparency = (if squareObj.Filled then convertTransparency(squareObj.Transparency) else 1)
			squareFrame.ZIndex = squareObj.ZIndex
			squareFrame.BackgroundColor3 = squareObj.Color
			squareFrame.Visible = squareObj.Visible

			uiStroke.Thickness = squareObj.Thickness
			uiStroke.Enabled = not squareObj.Filled
			uiStroke.LineJoinMode = Enum.LineJoinMode.Miter

			squareFrame.Parent, uiStroke.Parent = drawingUI, squareFrame
			local bs = table.create(0)
			table.insert(drawings,bs)
			return setmetatable(bs, {
				__newindex = function(_, index, value)
					if typeof(squareObj[index]) == "nil" then return end

					if index == "Size" then
						squareFrame.Size = UDim2.fromOffset(value.X, value.Y)
					elseif index == "Position" then
						squareFrame.Position = UDim2.fromOffset(value.X, value.Y)
					elseif index == "Thickness" then
						value = math.clamp(value, 0.6, 0x7fffffff)
						uiStroke.Thickness = value
					elseif index == "Filled" then
						squareFrame.BackgroundTransparency = (if value then convertTransparency(squareObj.Transparency) else 1)
						uiStroke.Enabled = not value
					elseif index == "Visible" then
						squareFrame.Visible = value
					elseif index == "ZIndex" then
						squareFrame.ZIndex = value
					elseif index == "Transparency" then
						local transparency = convertTransparency(value)

						squareFrame.BackgroundTransparency = (if squareObj.Filled then transparency else 1)
						uiStroke.Transparency = transparency
					elseif index == "Color" then
						uiStroke.Color = value
						squareFrame.BackgroundColor3 = value
					end
					squareObj[index] = value
				end,
				__index = function(self, index)
					if index == "Remove" or index == "Destroy" then
						return function()
							squareFrame:Destroy()
							squareObj.Remove(self)
							return squareObj:Remove()
						end
					end
					return squareObj[index]
				end
			})
		elseif drawingType == "Image" then
			local imageObj = ({
				Data = "",
				DataURL = "rbxassetid://0",
				Size = Vector2.zero,
				Position = Vector2.zero
			} + baseDrawingObj)

			local imageFrame = Instance.new("ImageLabel")
			imageFrame.Name = drawingIndex
			imageFrame.BorderSizePixel = 0
			imageFrame.ScaleType = Enum.ScaleType.Stretch
			imageFrame.BackgroundTransparency = 1

			imageFrame.Visible = imageObj.Visible
			imageFrame.ZIndex = imageObj.ZIndex
			imageFrame.ImageTransparency = convertTransparency(imageObj.Transparency)
			imageFrame.ImageColor3 = imageObj.Color

			imageFrame.Parent = drawingUI
			local bs = table.create(0)
			table.insert(drawings,bs)
			return setmetatable(bs, {
				__newindex = function(_, index, value)
					if typeof(imageObj[index]) == "nil" then return end

					if index == "Data" then
						-- later
					elseif index == "DataURL" then -- temporary property
						imageFrame.Image = value
					elseif index == "Size" then
						imageFrame.Size = UDim2.fromOffset(value.X, value.Y)
					elseif index == "Position" then
						imageFrame.Position = UDim2.fromOffset(value.X, value.Y)
					elseif index == "Visible" then
						imageFrame.Visible = value
					elseif index == "ZIndex" then
						imageFrame.ZIndex = value
					elseif index == "Transparency" then
						imageFrame.ImageTransparency = convertTransparency(value)
					elseif index == "Color" then
						imageFrame.ImageColor3 = value
					end
					imageObj[index] = value
				end,
				__index = function(self, index)
					if index == "Remove" or index == "Destroy" then
						return function()
							imageFrame:Destroy()
							imageObj.Remove(self)
							return imageObj:Remove()
						end
					elseif index == "Data" then
						return nil -- TODO: add error here
					end
					return imageObj[index]
				end
			})
		elseif drawingType == "Quad" then
			local quadObj = ({
				PointA = Vector2.zero,
				PointB = Vector2.zero,
				PointC = Vector2.zero,
				PointD = Vector3.zero,
				Thickness = 1,
				Filled = false
			} + baseDrawingObj)

			local _linePoints = table.create(0)
			_linePoints.A = DrawingLib.new("Line")
			_linePoints.B = DrawingLib.new("Line")
			_linePoints.C = DrawingLib.new("Line")
			_linePoints.D = DrawingLib.new("Line")
			local bs = table.create(0)
			table.insert(drawings,bs)
			return setmetatable(bs, {
				__newindex = function(_, index, value)
					if typeof(quadObj[index]) == "nil" then return end

					if index == "PointA" then
						_linePoints.A.From = value
						_linePoints.B.To = value
					elseif index == "PointB" then
						_linePoints.B.From = value
						_linePoints.C.To = value
					elseif index == "PointC" then
						_linePoints.C.From = value
						_linePoints.D.To = value
					elseif index == "PointD" then
						_linePoints.D.From = value
						_linePoints.A.To = value
					elseif (index == "Thickness" or index == "Visible" or index == "Color" or index == "ZIndex") then
						for _, linePoint in _linePoints do
							linePoint[index] = value
						end
					elseif index == "Filled" then
						-- later
					end
					quadObj[index] = value
				end,
				__index = function(self, index)
					if index == "Remove" then
						return function()
							for _, linePoint in _linePoints do
								linePoint:Remove()
							end

							quadObj.Remove(self)
							return quadObj:Remove()
						end
					end
					if index == "Destroy" then
						return function()
							for _, linePoint in _linePoints do
								linePoint:Remove()
							end

							quadObj.Remove(self)
							return quadObj:Remove()
						end
					end
					return quadObj[index]
				end
			})
		elseif drawingType == "Triangle" then
			local triangleObj = ({
				PointA = Vector2.zero,
				PointB = Vector2.zero,
				PointC = Vector2.zero,
				Thickness = 1,
				Filled = false
			} + baseDrawingObj)

			local _linePoints = table.create(0)
			_linePoints.A = DrawingLib.new("Line")
			_linePoints.B = DrawingLib.new("Line")
			_linePoints.C = DrawingLib.new("Line")
			local bs = table.create(0)
			table.insert(drawings,bs)
			return setmetatable(bs, {
				__newindex = function(_, index, value)
					if typeof(triangleObj[index]) == "nil" then return end

					if index == "PointA" then
						_linePoints.A.From = value
						_linePoints.B.To = value
					elseif index == "PointB" then
						_linePoints.B.From = value
						_linePoints.C.To = value
					elseif index == "PointC" then
						_linePoints.C.From = value
						_linePoints.A.To = value
					elseif (index == "Thickness" or index == "Visible" or index == "Color" or index == "ZIndex") then
						for _, linePoint in _linePoints do
							linePoint[index] = value
						end
					elseif index == "Filled" then
						-- later
					end
					triangleObj[index] = value
				end,
				__index = function(self, index)
					if index == "Remove" then
						return function()
							for _, linePoint in _linePoints do
								linePoint:Remove()
							end

							triangleObj.Remove(self)
							return triangleObj:Remove()
						end
					end
					if index == "Destroy" then
						return function()
							for _, linePoint in _linePoints do
								linePoint:Remove()
							end

							triangleObj.Remove(self)
							return triangleObj:Remove()
						end
					end
					return triangleObj[index]
				end
			})
		end
	end
	environment.global.Drawing = DrawingLib

	environment.global.getgenv = function()
		return cloned_environment.global
	end
	environment.global.getrenv = function()
		return environment.roblox
	end


	environment.global.crypt.base64encode = function(data)
		assert(data, "Missing #1 argument")
		assert(typeof(data) == "string", "Expected #1 argument to be string, got "..typeof(data).. " instead")

		local encoded = utils.base64_encode(data)

		return encoded
	end

	environment.global.crypt.base64decode = function(data)
		assert(data, "Missing #1 argument")
		assert(typeof(data) == "string", "Expected #1 argument to be string, got "..typeof(data).. " instead")

		local decoded = utils.base64_decode(data)

		return decoded
	end
	environment.global.crypt.generatekey = function(optionalSize)
 		local key = ''
 		local a = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
 		for i = 1, optionalSize or 32 do local n = math.random(1, #a) key = key .. a:sub(n, n) end
		return crypt.base64encode(key)
	end
	environment.global.crypt.generatebytes = function(size)
		if type(size) ~= 'number' then return error('missing arguement #1 to \'generatebytes\' (number expected)') end
		return crypt.generatekey(size)
	end
	local HashLib = setmetatable({}, {
	__metatable = 'HashLib // Protected',
	__index = function(self, key) -- Make it work for both _ and -
		local k1 = key:gsub('_', '-')
		local k2 = key:gsub('%-', '_')
		local m1, m2 = Hash[k1], Hash[k2]
		if m1 then return m1 end
		if m2 then return m2 end
		return rawget(self, key)
	end
})
	environment.global.crypt.hash = function(data, alg)
			local v1 = HashLib[alg]
			local v2 = HashLib[data]
			if not v1 and not v2 then
				error(string.format("No algorithm found with name '%s' or '%s'", alg, data))
			end
			if v1 then
				return v1(data)
			elseif v2 then
				return v2(alg)
			end
	end
	environment.global.crypt.encrypt = function(a, b)
	local result = {}
		a = tostring(a) b = tostring(b)
		for i = 1, #a do
			local byte = string.byte(a, i)
			local keyByte = string.byte(b, (i - 1) % #b + 1)
			table.insert(result, string.char(bit32.bxor(byte, keyByte)))
		end
		return table.concat(result)
	end
	environment.global.crypt.decrypt = environment.global.crypt.encrypt
	environment.global.crypt.random = function(len)
		return crypt.generatekey(len)
	end
	environment.global.crypt.base64.encode = environment.global.crypt.base64encode
	environment.global.crypt.base64_encode = environment.global.crypt.base64encode
	environment.global.base64.encode = environment.global.crypt.base64encode
	environment.global.base64encode = environment.global.crypt.base64encode
	environment.global.base64_encode = environment.global.crypt.base64encode
	environment.global.syn.crypt.base64.encode = environment.global.crypt.base64encode
	environment.global.fluxus.crypt.base64encode = environment.global.crypt.base64encode
	
	environment.global.rconsoleclear = function()
	end

	environment.global.consoleclear = environment.global.rconsoleclear

	environment.global.rconsolecreate = function()
	end

	environment.global.consolecreate = environment.global.rconsoleclear


	environment.global.rconsoledestroy = function()
	end

	environment.global.consoledestroy = environment.global.rconsoledestroy

	environment.global.rconsoleinput = function()
	end

	environment.global.consoleinput = environment.global.rconsoleinput

	environment.global.rconsoleprint = function()
	end

	environment.global.consoleprint = environment.global.rconsoleprint

	environment.global.rconsolesettitle = function()
	end

	environment.global.consolesettitle = environment.global.rconsolesettitle
	environment.global.rconsolename = environment.global.rconsolesettitle

	environment.global.crypt.base64.decode = environment.global.crypt.base64decode
	environment.global.crypt.base64_decode = environment.global.crypt.base64decode
	environment.global.base64.decode = environment.global.crypt.base64decode
	environment.global.base64decode = environment.global.crypt.base64decode
	environment.global.base64_decode = environment.global.crypt.base64decode
	environment.global.syn.crypt.base64.decode = environment.global.crypt.base64decode
	environment.global.fluxus.crypt.base64encode = environment.global.crypt.base64encode

	environment.global.isrenderobj = function(...)
		if table.find(drawings,...) then
			return true
		else
			return false
		end
	end
	environment.global.newcclosure = function(func)
		local func2 = nil
		func2 = function(...)
			environment.global[func] = coroutine.wrap(func2)
			return func(...)
		end
		func2 = coroutine.wrap(func2)
		return func2
	end
	environment.global.printidentity = function(phrase)
		local phrase = phrase or "Current identity is"
		print(phrase.. " ".. identity)
	end

	environment.global.getrenderproperty = function(a,b)
		return a[b]
	end
	environment.global.setrenderproperty = function(a,b,c)
		a[b] = c
	end

	environment.global.cleardrawcache = function()
		for _, v in pairs(game:GetDescendants()) do
			if v.Parent == drawingUI then
				v:Destroy()
			end
		end
		return true
	end

	environment.global.getscripthash = function(script)
		assert(typeof(script) == "Instance" and script:IsA("LuaSourceContainer"), "Invalid argument #1: Expected LuaSourceContainer, got ".. type(script), 0)
		return script:GetHash()
	end

	environment.global.base64_encode = environment.global.crypt.base64encode
	environment.global.base64_decode = environment.global.crypt.base64decode

	getfenv().identity = 6 -- im try to spoof ok sigma

	environment.global.identifyexecutor = function()
		return "BetterIncognito", bincogver()
	end

	environment.global.getthreadidentity = function()
		return identity
	end
	local Data = game:GetService("TeleportService"):GetLocalPlayerTeleportData()
		local TeleportData
		if Data and Data.MOREUNCSCRIPTQUEUE then
 			TeleportData = Data.MOREUNCSCRIPTQUEUE
		end
		if TeleportData then
 			local func = loadstring(TeleportData)
 			local s, e = pcall(func)
 			if not s then task.spawn(error, e) end
	end
	environment.global.queue_on_teleport = function(scripttoexec)
		local newTPService = {
		__index = function(self, key)
		if key == 'Teleport' then
			return function(gameId, player, teleportData, loadScreen)
			teleportData = {teleportData, MOREUNCSCRIPTQUEUE=scripttoexec}
			return oldGame:GetService("TeleportService"):Teleport(gameId, player, teleportData, loadScreen)
			end
		end
		end
		}
		local gameMeta = {
		__index = function(self, key)
			if key == 'GetService' then
			return function(name)
			if name == 'TeleportService' then return newTPService end
			end
			elseif key == 'TeleportService' then return newTPService end
			return game[key]
		end,
		__metatable = 'The metatable is protected'
		}
		getgenv().game = setmetatable({}, gameMeta)
		end
		environment.global.queueonteleport = environment.global.queue_on_teleport
	environment.global.getidentity = environment.global.getthreadidentity
	environment.global.syn.protect_gui = function(gui)
 		names[gui] = {name=gui.Name,parent=gui.Parent}
 		protecteduis[gui] = gui
 		gui.Name = environment.global.crypt.random(64) -- 64 byte string, removed hashing cuz its useless lmao
 		gui.Parent = gethui()
	end
	environment.global.syn.unprotect_gui = function(gui)
 	if names[gui] then gui.Name = names[gui].name gui.Parent = names[gui].parent end protecteduis[gui] = nil
	end
	environment.global.syn.protectgui = environment.global.syn.protect_gui
	environment.global.syn.unprotectgui = environment.global.syn.unprotect_gui
	environment.global.getthreadcontext = environment.global.getthreadidentity
	environment.global.syn.get_thread_identity = environment.global.getthreadidentity
	environment.global.get_thread_identity = environment.global.getthreadidentity
	environment.global.get_thread_context = environment.global.getthreadidentity
	environment.global.get_identity = environment.global.getthreadidentity
	environment.global.setsimulationradius = function(Distance, MaxDistance)
 		local LocalPlayer = game:GetService("Players").LocalPlayer
 		assert(type(Distance)=='number','Invalid arguement #1 to \'setsimulationradius\', Number expected got ' .. type(Distance))
 		LocalPlayer.SimulationRadius = type(Distance) == 'number' and Distance or LocalPlayer.SimulationRadius
 		if MaxDistance then
  			assert(type(MaxDistance)=='number','Invalid arguement #2 to \'setsimulationradius\', Number expected got ' .. type(MaxDistance))
  			LocalPlayer.MaxSimulationDistance = MaxDistance
 		end
	end
	environment.global.getexecutorname = function()
return "BetterIncognito"
end
	environment.global.iy = function()
    if IY_LOADED and not _G.IY_DEBUG == true then
      -- error("Infinite Yield is already running!", 0)
      return
  end
  
  pcall(function() getgenv().IY_LOADED = true end)
  
  local cloneref = cloneref or function(o) return o end
  COREGUI = cloneref(game:GetService("CoreGui"))
  Players = cloneref(game:GetService("Players"))
  
  if not game:IsLoaded() then
      local notLoaded = Instance.new("Message")
      notLoaded.Parent = COREGUI
      notLoaded.Text = "Infinite Yield is waiting for the game to load"
      game.Loaded:Wait()
      notLoaded:Destroy()
  end
  
  currentVersion = "5.9.7"
  
  Holder = Instance.new("Frame")
  Title = Instance.new("TextLabel")
  Dark = Instance.new("Frame")
  Cmdbar = Instance.new("TextBox")
  CMDsF = Instance.new("ScrollingFrame")
  cmdListLayout = Instance.new("UIListLayout")
  SettingsButton = Instance.new("ImageButton")
  ColorsButton = Instance.new("ImageButton")
  Settings = Instance.new("Frame")
  Prefix = Instance.new("TextLabel")
  PrefixBox = Instance.new("TextBox")
  Keybinds = Instance.new("TextLabel")
  StayOpen = Instance.new("TextLabel")
  Button = Instance.new("Frame")
  On = Instance.new("TextButton")
  Positions = Instance.new("TextLabel")
  EventBind = Instance.new("TextLabel")
  Plugins = Instance.new("TextLabel")
  Example = Instance.new("TextButton")
  Notification = Instance.new("Frame")
  Title_2 = Instance.new("TextLabel")
  Text_2 = Instance.new("TextLabel")
  CloseButton = Instance.new("TextButton")
  CloseImage = Instance.new("ImageLabel")
  PinButton = Instance.new("TextButton")
  PinImage = Instance.new("ImageLabel")
  Tooltip = Instance.new("Frame")
  Title_3 = Instance.new("TextLabel")
  Description = Instance.new("TextLabel")
  IntroBackground = Instance.new("Frame")
  Logo = Instance.new("ImageLabel")
  Credits = Instance.new("TextBox")
  KeybindsFrame = Instance.new("Frame")
  Close = Instance.new("TextButton")
  Add = Instance.new("TextButton")
  Delete = Instance.new("TextButton")
  Holder_2 = Instance.new("ScrollingFrame")
  Example_2 = Instance.new("Frame")
  Text_3 = Instance.new("TextLabel")
  Delete_2 = Instance.new("TextButton")
  KeybindEditor = Instance.new("Frame")
  background_2 = Instance.new("Frame")
  Dark_3 = Instance.new("Frame")
  Directions = Instance.new("TextLabel")
  BindTo = Instance.new("TextButton")
  TriggerLabel = Instance.new("TextLabel")
  BindTriggerSelect = Instance.new("TextButton")
  Add_2 = Instance.new("TextButton")
  Toggles = Instance.new("ScrollingFrame")
  ClickTP  = Instance.new("TextLabel")
  Select = Instance.new("TextButton")
  ClickDelete = Instance.new("TextLabel")
  Select_2 = Instance.new("TextButton")
  Cmdbar_2 = Instance.new("TextBox")
  Cmdbar_3 = Instance.new("TextBox")
  CreateToggle = Instance.new("TextLabel")
  Button_2 = Instance.new("Frame")
  On_2 = Instance.new("TextButton")
  shadow_2 = Instance.new("Frame")
  PopupText_2 = Instance.new("TextLabel")
  Exit_2 = Instance.new("TextButton")
  ExitImage_2 = Instance.new("ImageLabel")
  PositionsFrame = Instance.new("Frame")
  Close_3 = Instance.new("TextButton")
  Delete_5 = Instance.new("TextButton")
  Part = Instance.new("TextButton")
  Holder_4 = Instance.new("ScrollingFrame")
  Example_4 = Instance.new("Frame")
  Text_5 = Instance.new("TextLabel")
  Delete_6 = Instance.new("TextButton")
  TP = Instance.new("TextButton")
  AliasesFrame = Instance.new("Frame")
  Close_2 = Instance.new("TextButton")
  Delete_3 = Instance.new("TextButton")
  Holder_3 = Instance.new("ScrollingFrame")
  Example_3 = Instance.new("Frame")
  Text_4 = Instance.new("TextLabel")
  Delete_4 = Instance.new("TextButton")
  Aliases = Instance.new("TextLabel")
  PluginsFrame = Instance.new("Frame")
  Close_4 = Instance.new("TextButton")
  Add_3 = Instance.new("TextButton")
  Holder_5 = Instance.new("ScrollingFrame")
  Example_5 = Instance.new("Frame")
  Text_6 = Instance.new("TextLabel")
  Delete_7 = Instance.new("TextButton")
  PluginEditor = Instance.new("Frame")
  background_3 = Instance.new("Frame")
  Dark_2 = Instance.new("Frame")
  Img = Instance.new("ImageButton")
  AddPlugin = Instance.new("TextButton")
  FileName = Instance.new("TextBox")
  About = Instance.new("TextLabel")
  Directions_2 = Instance.new("TextLabel")
  shadow_3 = Instance.new("Frame")
  PopupText_3 = Instance.new("TextLabel")
  Exit_3 = Instance.new("TextButton")
  ExitImage_3 = Instance.new("ImageLabel")
  AliasHint = Instance.new("TextLabel")
  PluginsHint = Instance.new("TextLabel")
  PositionsHint = Instance.new("TextLabel")
  ToPartFrame = Instance.new("Frame")
  background_4 = Instance.new("Frame")
  ChoosePart = Instance.new("TextButton")
  CopyPath = Instance.new("TextButton")
  Directions_3 = Instance.new("TextLabel")
  Path = Instance.new("TextLabel")
  shadow_4 = Instance.new("Frame")
  PopupText_5 = Instance.new("TextLabel")
  Exit_4 = Instance.new("TextButton")
  ExitImage_5 = Instance.new("ImageLabel")
  logs = Instance.new("Frame")
  shadow = Instance.new("Frame")
  Hide = Instance.new("TextButton")
  ImageLabel = Instance.new("ImageLabel")
  PopupText = Instance.new("TextLabel")
  Exit = Instance.new("TextButton")
  ImageLabel_2 = Instance.new("ImageLabel")
  background = Instance.new("Frame")
  chat = Instance.new("Frame")
  Clear = Instance.new("TextButton")
  SaveChatlogs = Instance.new("TextButton")
  Toggle = Instance.new("TextButton")
  scroll_2 = Instance.new("ScrollingFrame")
  join = Instance.new("Frame")
  Toggle_2 = Instance.new("TextButton")
  Clear_2 = Instance.new("TextButton")
  scroll_3 = Instance.new("ScrollingFrame")
  listlayout = Instance.new("UIListLayout",scroll_3)
  selectChat = Instance.new("TextButton")
  selectJoin = Instance.new("TextButton")
  
  function randomString()
    local length = math.random(10,20)
    local array = {}
    for i = 1, length do
      array[i] = string.char(math.random(32, 126))
    end
    return table.concat(array)
  end
  
  PARENT = nil
  if get_hidden_gui or gethui then
    local hiddenUI = get_hidden_gui or gethui
    local Main = Instance.new("ScreenGui")
    Main.Name = randomString()
    Main.Parent = hiddenUI()
    PARENT = Main
  elseif (not is_sirhurt_closure) and (syn and syn.protect_gui) then
    local Main = Instance.new("ScreenGui")
    Main.Name = randomString()
    syn.protect_gui(Main)
    Main.Parent = COREGUI
    PARENT = Main
  elseif COREGUI:FindFirstChild('RobloxGui') then
    PARENT = COREGUI.RobloxGui
  else
    local Main = Instance.new("ScreenGui")
    Main.Name = randomString()
    Main.Parent = COREGUI
    PARENT = Main
  end
  
  shade1 = {}
  shade2 = {}
  shade3 = {}
  text1 = {}
  text2 = {}
  scroll = {}
  
  Holder.Name = randomString()
  Holder.Parent = PARENT
  Holder.Active = true
  Holder.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Holder.BorderSizePixel = 0
  Holder.Position = UDim2.new(1, -250, 1, -220)
  Holder.Size = UDim2.new(0, 250, 0, 220)
  Holder.ZIndex = 10
  table.insert(shade2,Holder)
  
  Title.Name = "Title"
  Title.Parent = Holder
  Title.Active = true
  Title.BackgroundColor3 = Color3.fromRGB(36,36,37)
  Title.BorderSizePixel = 0
  Title.Size = UDim2.new(0, 250, 0, 20)
  Title.Font = Enum.Font.SourceSans
  Title.TextSize = 18
  Title.Text = "Infinite Yield FE v" .. currentVersion
  
  do
    local emoji = ({
      ["01 01"] = "🎆",
      [(function(Year)
        local A = math.floor(Year/100)
        local B = math.floor((13+8*A)/25)
        local C = (15-B+A-math.floor(A/4))%30
        local D = (4+A-math.floor(A/4))%7
        local E = (19*(Year%19)+C)%30
        local F = (2*(Year%4)+4*(Year%7)+6*E+D)%7
        local G = (22+E+F)
        if E == 29 and F == 6 then
          return "04 19"
        elseif E == 28 and F == 6 then
          return "04 18"
        elseif 31 < G then
          return ("04 %02d"):format(G-31)
        end
        return ("03 %02d"):format(G)
      end)(tonumber(os.date"%Y"))] = "🥚",
      ["10 31"] = "🎃",
      ["12 25"] = "🎄"
    })[os.date("%m %d")]
    if emoji then
      Title.Text = ("%s %s %s"):format(emoji, Title.Text, emoji)
    end
  end
  
  Title.TextColor3 = Color3.new(1, 1, 1)
  Title.ZIndex = 10
  table.insert(shade1,Title)
  table.insert(text1,Title)
  
  Dark.Name = "Dark"
  Dark.Parent = Holder
  Dark.Active = true
  Dark.BackgroundColor3 = Color3.fromRGB(36, 36, 37)
  Dark.BorderSizePixel = 0
  Dark.Position = UDim2.new(0, 0, 0, 45)
  Dark.Size = UDim2.new(0, 250, 0, 175)
  Dark.ZIndex = 10
  table.insert(shade1,Dark)
  
  Cmdbar.Name = "Cmdbar"
  Cmdbar.Parent = Holder
  Cmdbar.BackgroundTransparency = 1
  Cmdbar.BorderSizePixel = 0
  Cmdbar.Position = UDim2.new(0, 5, 0, 20)
  Cmdbar.Size = UDim2.new(0, 240, 0, 25)
  Cmdbar.Font = Enum.Font.SourceSans
  Cmdbar.TextSize = 18
  Cmdbar.TextXAlignment = Enum.TextXAlignment.Left
  Cmdbar.TextColor3 = Color3.new(1, 1, 1)
  Cmdbar.Text = ""
  Cmdbar.ZIndex = 10
  Cmdbar.PlaceholderText = "Command Bar"
  
  CMDsF.Name = "CMDs"
  CMDsF.Parent = Holder
  CMDsF.BackgroundTransparency = 1
  CMDsF.BorderSizePixel = 0
  CMDsF.Position = UDim2.new(0, 5, 0, 45)
  CMDsF.Size = UDim2.new(0, 245, 0, 175)
  CMDsF.ScrollBarImageColor3 = Color3.fromRGB(78,78,79)
  CMDsF.BottomImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  CMDsF.CanvasSize = UDim2.new(0, 0, 0, 0)
  CMDsF.MidImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  CMDsF.ScrollBarThickness = 8
  CMDsF.TopImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  CMDsF.VerticalScrollBarInset = 'Always'
  CMDsF.ZIndex = 10
  table.insert(scroll,CMDsF)
  
  cmdListLayout.Parent = CMDsF
  
  SettingsButton.Name = "SettingsButton"
  SettingsButton.Parent = Holder
  SettingsButton.BackgroundTransparency = 1
  SettingsButton.Position = UDim2.new(0, 230, 0, 0)
  SettingsButton.Size = UDim2.new(0, 20, 0, 20)
  SettingsButton.Image = "rbxassetid://1204397029"
  SettingsButton.ZIndex = 10
  
  ReferenceButton = Instance.new("ImageButton")
  ReferenceButton.Name = "ReferenceButton"
  ReferenceButton.Parent = Holder
  ReferenceButton.BackgroundTransparency = 1
  ReferenceButton.Position = UDim2.new(0, 212, 0, 2)
  ReferenceButton.Size = UDim2.new(0, 16, 0, 16)
  ReferenceButton.Image = "rbxassetid://3523243755"
  ReferenceButton.ZIndex = 10
  
  Settings.Name = "Settings"
  Settings.Parent = Holder
  Settings.Active = true
  Settings.BackgroundColor3 = Color3.fromRGB(36, 36, 37)
  Settings.BorderSizePixel = 0
  Settings.Position = UDim2.new(0, 0, 0, 220)
  Settings.Size = UDim2.new(0, 250, 0, 175)
  Settings.ZIndex = 10
  table.insert(shade1,Settings)
  
  SettingsHolder = Instance.new("ScrollingFrame")
  SettingsHolder.Name = "Holder"
  SettingsHolder.Parent = Settings
  SettingsHolder.BackgroundTransparency = 1
  SettingsHolder.BorderSizePixel = 0
  SettingsHolder.Size = UDim2.new(1,0,1,0)
  SettingsHolder.ScrollBarImageColor3 = Color3.fromRGB(78,78,79)
  SettingsHolder.BottomImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  SettingsHolder.CanvasSize = UDim2.new(0, 0, 0, 235)
  SettingsHolder.MidImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  SettingsHolder.ScrollBarThickness = 8
  SettingsHolder.TopImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  SettingsHolder.VerticalScrollBarInset = 'Always'
  SettingsHolder.ZIndex = 10
  table.insert(scroll,SettingsHolder)
  
  Prefix.Name = "Prefix"
  Prefix.Parent = SettingsHolder
  Prefix.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Prefix.BorderSizePixel = 0
  Prefix.BackgroundTransparency = 1
  Prefix.Position = UDim2.new(0, 5, 0, 5)
  Prefix.Size = UDim2.new(1, -10, 0, 20)
  Prefix.Font = Enum.Font.SourceSans
  Prefix.TextSize = 14
  Prefix.Text = "Prefix"
  Prefix.TextColor3 = Color3.new(1, 1, 1)
  Prefix.TextXAlignment = Enum.TextXAlignment.Left
  Prefix.ZIndex = 10
  table.insert(shade2,Prefix)
  table.insert(text1,Prefix)
  
  PrefixBox.Name = "PrefixBox"
  PrefixBox.Parent = Prefix
  PrefixBox.BackgroundColor3 = Color3.fromRGB(78, 78, 79)
  PrefixBox.BorderSizePixel = 0
  PrefixBox.Position = UDim2.new(1, -20, 0, 0)
  PrefixBox.Size = UDim2.new(0, 20, 0, 20)
  PrefixBox.Font = Enum.Font.SourceSansBold
  PrefixBox.TextSize = 14
  PrefixBox.Text = ''
  PrefixBox.TextColor3 = Color3.new(0, 0, 0)
  PrefixBox.ZIndex = 10
  table.insert(shade3,PrefixBox)
  table.insert(text2,PrefixBox)
  
  function makeSettingsButton(name,iconID,off)
    local button = Instance.new("TextButton")
    button.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
    button.BorderSizePixel = 0
    button.Position = UDim2.new(0,0,0,0)
    button.Size = UDim2.new(1,0,0,25)
    button.Text = ""
    button.ZIndex = 10
    local icon = Instance.new("ImageLabel")
    icon.Name = "Icon"
    icon.Parent = button
    icon.Position = UDim2.new(0,5,0,5)
    icon.Size = UDim2.new(0,16,0,16)
    icon.BackgroundTransparency = 1
    icon.Image = iconID
    icon.ZIndex = 10
    if off then
      icon.ScaleType = Enum.ScaleType.Crop
      icon.ImageRectSize = Vector2.new(16,16)
      icon.ImageRectOffset = Vector2.new(off,0)
    end
    local label = Instance.new("TextLabel")
    label.Name = "ButtonLabel"
    label.Parent = button
    label.BackgroundTransparency = 1
    label.Text = name
    label.Position = UDim2.new(0,28,0,0)
    label.Size = UDim2.new(1,-28,1,0)
    label.Font = Enum.Font.SourceSans
    label.TextColor3 = Color3.new(1, 1, 1)
    label.TextSize = 14
    label.ZIndex = 10
    label.TextXAlignment = Enum.TextXAlignment.Left
    table.insert(shade2,button)
    table.insert(text1,label)
    return button
  end
  
  ColorsButton = makeSettingsButton("Edit Theme","rbxassetid://4911962991")
  ColorsButton.Position = UDim2.new(0,5,0,55)
  ColorsButton.Size = UDim2.new(1,-10,0,25)
  ColorsButton.Name = "Colors"
  ColorsButton.Parent = SettingsHolder
  
  Keybinds = makeSettingsButton("Edit Keybinds","rbxassetid://129697930")
  Keybinds.Position = UDim2.new(0, 5, 0, 85)
  Keybinds.Size = UDim2.new(1, -10, 0, 25)
  Keybinds.Name = "Keybinds"
  Keybinds.Parent = SettingsHolder
  
  Aliases = makeSettingsButton("Edit Aliases","rbxassetid://5147488658")
  Aliases.Position = UDim2.new(0, 5, 0, 115)
  Aliases.Size = UDim2.new(1, -10, 0, 25)
  Aliases.Name = "Aliases"
  Aliases.Parent = SettingsHolder
  
  StayOpen.Name = "StayOpen"
  StayOpen.Parent = SettingsHolder
  StayOpen.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  StayOpen.BorderSizePixel = 0
  StayOpen.BackgroundTransparency = 1
  StayOpen.Position = UDim2.new(0, 5, 0, 30)
  StayOpen.Size = UDim2.new(1, -10, 0, 20)
  StayOpen.Font = Enum.Font.SourceSans
  StayOpen.TextSize = 14
  StayOpen.Text = "Keep Menu Open"
  StayOpen.TextColor3 = Color3.new(1, 1, 1)
  StayOpen.TextXAlignment = Enum.TextXAlignment.Left
  StayOpen.ZIndex = 10
  table.insert(shade2,StayOpen)
  table.insert(text1,StayOpen)
  
  Button.Name = "Button"
  Button.Parent = StayOpen
  Button.BackgroundColor3 = Color3.fromRGB(78, 78, 79)
  Button.BorderSizePixel = 0
  Button.Position = UDim2.new(1, -20, 0, 0)
  Button.Size = UDim2.new(0, 20, 0, 20)
  Button.ZIndex = 10
  table.insert(shade3,Button)
  
  On.Name = "On"
  On.Parent = Button
  On.BackgroundColor3 = Color3.fromRGB(150, 150, 151)
  On.BackgroundTransparency = 1
  On.BorderSizePixel = 0
  On.Position = UDim2.new(0, 2, 0, 2)
  On.Size = UDim2.new(0, 16, 0, 16)
  On.Font = Enum.Font.SourceSans
  On.FontSize = Enum.FontSize.Size14
  On.Text = ""
  On.TextColor3 = Color3.new(0, 0, 0)
  On.ZIndex = 10
  
  Positions = makeSettingsButton("Edit/Goto Waypoints","rbxassetid://5147488592")
  Positions.Position = UDim2.new(0, 5, 0, 145)
  Positions.Size = UDim2.new(1, -10, 0, 25)
  Positions.Name = "Waypoints"
  Positions.Parent = SettingsHolder
  
  EventBind = makeSettingsButton("Edit Event Binds","rbxassetid://5147695474",759)
  EventBind.Position = UDim2.new(0, 5, 0, 205)
  EventBind.Size = UDim2.new(1, -10, 0, 25)
  EventBind.Name = "EventBinds"
  EventBind.Parent = SettingsHolder
  
  Plugins = makeSettingsButton("Manage Plugins","rbxassetid://5147695474",743)
  Plugins.Position = UDim2.new(0, 5, 0, 175)
  Plugins.Size = UDim2.new(1, -10, 0, 25)
  Plugins.Name = "Plugins"
  Plugins.Parent = SettingsHolder
  
  Example.Name = "Example"
  Example.Parent = Holder
  Example.BackgroundTransparency = 1
  Example.BorderSizePixel = 0
  Example.Size = UDim2.new(0, 190, 0, 20)
  Example.Visible = false
  Example.Font = Enum.Font.SourceSans
  Example.TextSize = 18
  Example.Text = "Example"
  Example.TextColor3 = Color3.new(1, 1, 1)
  Example.TextXAlignment = Enum.TextXAlignment.Left
  Example.ZIndex = 10
  table.insert(text1,Example)
  
  Notification.Name = randomString()
  Notification.Parent = PARENT
  Notification.BackgroundColor3 = Color3.fromRGB(36, 36, 37)
  Notification.BorderSizePixel = 0
  Notification.Position = UDim2.new(1, -500, 1, 20)
  Notification.Size = UDim2.new(0, 250, 0, 100)
  Notification.ZIndex = 10
  table.insert(shade1,Notification)
  
  Title_2.Name = "Title"
  Title_2.Parent = Notification
  Title_2.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Title_2.BorderSizePixel = 0
  Title_2.Size = UDim2.new(0, 250, 0, 20)
  Title_2.Font = Enum.Font.SourceSans
  Title_2.TextSize = 14
  Title_2.Text = "Notification Title"
  Title_2.TextColor3 = Color3.new(1, 1, 1)
  Title_2.ZIndex = 10
  table.insert(shade2,Title_2)
  table.insert(text1,Title_2)
  
  Text_2.Name = "Text"
  Text_2.Parent = Notification
  Text_2.BackgroundTransparency = 1
  Text_2.BorderSizePixel = 0
  Text_2.Position = UDim2.new(0, 5, 0, 25)
  Text_2.Size = UDim2.new(0, 240, 0, 75)
  Text_2.Font = Enum.Font.SourceSans
  Text_2.TextSize = 16
  Text_2.Text = "Notification Text"
  Text_2.TextColor3 = Color3.new(1, 1, 1)
  Text_2.TextWrapped = true
  Text_2.ZIndex = 10
  table.insert(text1,Text_2)
  
  CloseButton.Name = "CloseButton"
  CloseButton.Parent = Notification
  CloseButton.BackgroundTransparency = 1
  CloseButton.Position = UDim2.new(1, -20, 0, 0)
  CloseButton.Size = UDim2.new(0, 20, 0, 20)
  CloseButton.Text = ""
  CloseButton.ZIndex = 10
  
  CloseImage.Parent = CloseButton
  CloseImage.BackgroundColor3 = Color3.new(1, 1, 1)
  CloseImage.BackgroundTransparency = 1
  CloseImage.Position = UDim2.new(0, 5, 0, 5)
  CloseImage.Size = UDim2.new(0, 10, 0, 10)
  CloseImage.Image = "rbxassetid://5054663650"
  CloseImage.ZIndex = 10
  
  PinButton.Name = "PinButton"
  PinButton.Parent = Notification
  PinButton.BackgroundTransparency = 1
  PinButton.Size = UDim2.new(0, 20, 0, 20)
  PinButton.ZIndex = 10
  PinButton.Text = ""
  
  PinImage.Parent = PinButton
  PinImage.BackgroundColor3 = Color3.new(1, 1, 1)
  PinImage.BackgroundTransparency = 1
  PinImage.Position = UDim2.new(0, 3, 0, 3)
  PinImage.Size = UDim2.new(0, 14, 0, 14)
  PinImage.ZIndex = 10
  PinImage.Image = "rbxassetid://6234691350"
  
  Tooltip.Name = randomString()
  Tooltip.Parent = PARENT
  Tooltip.Active = true
  Tooltip.BackgroundColor3 = Color3.fromRGB(36, 36, 37)
  Tooltip.BackgroundTransparency = 0.1
  Tooltip.BorderSizePixel = 0
  Tooltip.Size = UDim2.new(0, 200, 0, 96)
  Tooltip.Visible = false
  Tooltip.ZIndex = 10
  table.insert(shade1,Tooltip)
  
  Title_3.Name = "Title"
  Title_3.Parent = Tooltip
  Title_3.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Title_3.BackgroundTransparency = 0.1
  Title_3.BorderSizePixel = 0
  Title_3.Size = UDim2.new(0, 200, 0, 20)
  Title_3.Font = Enum.Font.SourceSans
  Title_3.TextSize = 14
  Title_3.Text = ""
  Title_3.TextColor3 = Color3.new(1, 1, 1)
  Title_3.TextTransparency = 0.1
  Title_3.ZIndex = 10
  table.insert(shade2,Title_3)
  table.insert(text1,Title_3)
  
  Description.Name = "Description"
  Description.Parent = Tooltip
  Description.BackgroundTransparency = 1
  Description.BorderSizePixel = 0
  Description.Size = UDim2.new(0,180,0,72)
  Description.Position = UDim2.new(0,10,0,18)
  Description.Font = Enum.Font.SourceSans
  Description.TextSize = 16
  Description.Text = ""
  Description.TextColor3 = Color3.new(1, 1, 1)
  Description.TextTransparency = 0.1
  Description.TextWrapped = true
  Description.ZIndex = 10
  table.insert(text1,Description)
  
  IntroBackground.Name = "IntroBackground"
  IntroBackground.Parent = Holder
  IntroBackground.Active = true
  IntroBackground.BackgroundColor3 = Color3.fromRGB(36, 36, 37)
  IntroBackground.BorderSizePixel = 0
  IntroBackground.Position = UDim2.new(0, 0, 0, 45)
  IntroBackground.Size = UDim2.new(0, 250, 0, 175)
  IntroBackground.ZIndex = 10
  
  Logo.Name = "Logo"
  Logo.Parent = Holder
  Logo.BackgroundTransparency = 1
  Logo.BorderSizePixel = 0
  Logo.Position = UDim2.new(0, 125, 0, 127)
  Logo.Size = UDim2.new(0, 10, 0, 10)
  Logo.Image = "rbxassetid://1352543873"
  Logo.ImageTransparency = 0
  Logo.ZIndex = 10
  
  Credits.Name = "Credits"
  Credits.Parent = Holder
  Credits.BackgroundTransparency = 1
  Credits.BorderSizePixel = 0
  Credits.Position = UDim2.new(0, 0, 0.9, 30)
  Credits.Size = UDim2.new(0, 250, 0, 20)
  Credits.Font = Enum.Font.SourceSansLight
  Credits.FontSize = Enum.FontSize.Size18
  Credits.Text = "Edge // Zwolf // Moon // Toon"
  Credits.TextColor3 = Color3.new(1, 1, 1)
  Credits.ZIndex = 10
  
  KeybindsFrame.Name = "KeybindsFrame"
  KeybindsFrame.Parent = Settings
  KeybindsFrame.Active = true
  KeybindsFrame.BackgroundColor3 = Color3.fromRGB(36, 36, 37)
  KeybindsFrame.BorderSizePixel = 0
  KeybindsFrame.Position = UDim2.new(0, 0, 0, 175)
  KeybindsFrame.Size = UDim2.new(0, 250, 0, 175)
  KeybindsFrame.ZIndex = 10
  table.insert(shade1,KeybindsFrame)
  
  Close.Name = "Close"
  Close.Parent = KeybindsFrame
  Close.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Close.BorderSizePixel = 0
  Close.Position = UDim2.new(0, 205, 0, 150)
  Close.Size = UDim2.new(0, 40, 0, 20)
  Close.Font = Enum.Font.SourceSans
  Close.TextSize = 14
  Close.Text = "Close"
  Close.TextColor3 = Color3.new(1, 1, 1)
  Close.ZIndex = 10
  table.insert(shade2,Close)
  table.insert(text1,Close)
  
  Add.Name = "Add"
  Add.Parent = KeybindsFrame
  Add.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Add.BorderSizePixel = 0
  Add.Position = UDim2.new(0, 5, 0, 150)
  Add.Size = UDim2.new(0, 40, 0, 20)
  Add.Font = Enum.Font.SourceSans
  Add.TextSize = 14
  Add.Text = "Add"
  Add.TextColor3 = Color3.new(1, 1, 1)
  Add.ZIndex = 10
  table.insert(shade2,Add)
  table.insert(text1,Add)
  
  Delete.Name = "Delete"
  Delete.Parent = KeybindsFrame
  Delete.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Delete.BorderSizePixel = 0
  Delete.Position = UDim2.new(0, 50, 0, 150)
  Delete.Size = UDim2.new(0, 40, 0, 20)
  Delete.Font = Enum.Font.SourceSans
  Delete.TextSize = 14
  Delete.Text = "Clear"
  Delete.TextColor3 = Color3.new(1, 1, 1)
  Delete.ZIndex = 10
  table.insert(shade2,Delete)
  table.insert(text1,Delete)
  
  Holder_2.Name = "Holder"
  Holder_2.Parent = KeybindsFrame
  Holder_2.BackgroundTransparency = 1
  Holder_2.BorderSizePixel = 0
  Holder_2.Position = UDim2.new(0, 0, 0, 0)
  Holder_2.Size = UDim2.new(0, 250, 0, 145)
  Holder_2.ScrollBarImageColor3 = Color3.fromRGB(78,78,79)
  Holder_2.BottomImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  Holder_2.CanvasSize = UDim2.new(0, 0, 0, 0)
  Holder_2.MidImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  Holder_2.ScrollBarThickness = 0
  Holder_2.TopImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  Holder_2.VerticalScrollBarInset = 'Always'
  Holder_2.ZIndex = 10
  
  Example_2.Name = "Example"
  Example_2.Parent = KeybindsFrame
  Example_2.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Example_2.BorderSizePixel = 0
  Example_2.Size = UDim2.new(0, 10, 0, 20)
  Example_2.Visible = false
  Example_2.ZIndex = 10
  table.insert(shade2,Example_2)
  
  Text_3.Name = "Text"
  Text_3.Parent = Example_2
  Text_3.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Text_3.BorderSizePixel = 0
  Text_3.Position = UDim2.new(0, 10, 0, 0)
  Text_3.Size = UDim2.new(0, 240, 0, 20)
  Text_3.Font = Enum.Font.SourceSans
  Text_3.TextSize = 14
  Text_3.Text = "nom"
  Text_3.TextColor3 = Color3.new(1, 1, 1)
  Text_3.TextXAlignment = Enum.TextXAlignment.Left
  Text_3.ZIndex = 10
  table.insert(shade2,Text_3)
  table.insert(text1,Text_3)
  
  Delete_2.Name = "Delete"
  Delete_2.Parent = Text_3
  Delete_2.BackgroundColor3 = Color3.fromRGB(78, 78, 79)
  Delete_2.BorderSizePixel = 0
  Delete_2.Position = UDim2.new(0, 200, 0, 0)
  Delete_2.Size = UDim2.new(0, 40, 0, 20)
  Delete_2.Font = Enum.Font.SourceSans
  Delete_2.TextSize = 14
  Delete_2.Text = "Delete"
  Delete_2.TextColor3 = Color3.new(0, 0, 0)
  Delete_2.ZIndex = 10
  table.insert(shade3,Delete_2)
  table.insert(text2,Delete_2)
  
  KeybindEditor.Name = randomString()
  KeybindEditor.Parent = PARENT
  KeybindEditor.Active = true
  KeybindEditor.BackgroundTransparency = 1
  KeybindEditor.Position = UDim2.new(0.5, -180, 0, -500)
  KeybindEditor.Size = UDim2.new(0, 360, 0, 20)
  KeybindEditor.ZIndex = 10
  
  background_2.Name = "background"
  background_2.Parent = KeybindEditor
  background_2.Active = true
  background_2.BackgroundColor3 = Color3.fromRGB(36, 36, 37)
  background_2.BorderSizePixel = 0
  background_2.Position = UDim2.new(0, 0, 0, 20)
  background_2.Size = UDim2.new(0, 360, 0, 185)
  background_2.ZIndex = 10
  table.insert(shade1,background_2)
  
  Dark_3.Name = "Dark"
  Dark_3.Parent = background_2
  Dark_3.Active = true
  Dark_3.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Dark_3.BorderSizePixel = 0
  Dark_3.Position = UDim2.new(0, 135, 0, 0)
  Dark_3.Size = UDim2.new(0, 2, 0, 185)
  Dark_3.ZIndex = 10
  table.insert(shade2,Dark_3)
  
  Directions.Name = "Directions"
  Directions.Parent = background_2
  Directions.BackgroundTransparency = 1
  Directions.BorderSizePixel = 0
  Directions.Position = UDim2.new(0, 10, 0, 15)
  Directions.Size = UDim2.new(0, 115, 0, 90)
  Directions.ZIndex = 10
  Directions.Font = Enum.Font.SourceSans
  Directions.Text = "Click the button below and press a key/mouse button. Then select what you want to bind it to."
  Directions.TextColor3 = Color3.fromRGB(255, 255, 255)
  Directions.TextSize = 14.000
  Directions.TextWrapped = true
  Directions.TextYAlignment = Enum.TextYAlignment.Top
  table.insert(text1,Directions)
  
  BindTo.Name = "BindTo"
  BindTo.Parent = background_2
  BindTo.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  BindTo.BorderSizePixel = 0
  BindTo.Position = UDim2.new(0, 10, 0, 95)
  BindTo.Size = UDim2.new(0, 115, 0, 50)
  BindTo.ZIndex = 10
  BindTo.Font = Enum.Font.SourceSans
  BindTo.Text = "Click to bind"
  BindTo.TextColor3 = Color3.fromRGB(255, 255, 255)
  BindTo.TextSize = 16.000
  table.insert(shade2,BindTo)
  table.insert(text1,BindTo)
  
  TriggerLabel.Name = "TriggerLabel"
  TriggerLabel.Parent = background_2
  TriggerLabel.BackgroundTransparency = 1
  TriggerLabel.Position = UDim2.new(0, 10, 0, 155)
  TriggerLabel.Size = UDim2.new(0, 45, 0, 20)
  TriggerLabel.ZIndex = 10
  TriggerLabel.Font = Enum.Font.SourceSans
  TriggerLabel.Text = "Trigger:"
  TriggerLabel.TextColor3 = Color3.fromRGB(255, 255, 255)
  TriggerLabel.TextSize = 14.000
  TriggerLabel.TextXAlignment = Enum.TextXAlignment.Left
  table.insert(text1,TriggerLabel)
  
  BindTriggerSelect.Name = "BindTo"
  BindTriggerSelect.Parent = background_2
  BindTriggerSelect.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  BindTriggerSelect.BorderSizePixel = 0
  BindTriggerSelect.Position = UDim2.new(0, 60, 0, 155)
  BindTriggerSelect.Size = UDim2.new(0, 65, 0, 20)
  BindTriggerSelect.ZIndex = 10
  BindTriggerSelect.Font = Enum.Font.SourceSans
  BindTriggerSelect.Text = "KeyDown"
  BindTriggerSelect.TextColor3 = Color3.fromRGB(255, 255, 255)
  BindTriggerSelect.TextSize = 16.000
  table.insert(shade2,BindTriggerSelect)
  table.insert(text1,BindTriggerSelect)
  
  Add_2.Name = "Add"
  Add_2.Parent = background_2
  Add_2.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Add_2.BorderSizePixel = 0
  Add_2.Position = UDim2.new(0, 310, 0, 35)
  Add_2.Size = UDim2.new(0, 40, 0, 20)
  Add_2.ZIndex = 10
  Add_2.Font = Enum.Font.SourceSans
  Add_2.Text = "Add"
  Add_2.TextColor3 = Color3.fromRGB(255, 255, 255)
  Add_2.TextSize = 14.000
  table.insert(shade2,Add_2)
  table.insert(text1,Add_2)
  
  Toggles.Name = "Toggles"
  Toggles.Parent = background_2
  Toggles.BackgroundTransparency = 1
  Toggles.BorderSizePixel = 0
  Toggles.Position = UDim2.new(0, 150, 0, 125)
  Toggles.Size = UDim2.new(0, 200, 0, 50)
  Toggles.ZIndex = 10
  Toggles.BottomImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  Toggles.CanvasSize = UDim2.new(0, 0, 0, 50)
  Toggles.ScrollBarThickness = 8
  Toggles.TopImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  Toggles.VerticalScrollBarInset = Enum.ScrollBarInset.Always
  table.insert(scroll,Toggles)
  
  ClickTP.Name = "Click TP (Hold Key & Click)"
  ClickTP.Parent = Toggles
  ClickTP.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  ClickTP.BorderSizePixel = 0
  ClickTP.Size = UDim2.new(0, 200, 0, 20)
  ClickTP.ZIndex = 10
  ClickTP.Font = Enum.Font.SourceSans
  ClickTP.Text = "    Click TP (Hold Key & Click)"
  ClickTP.TextColor3 = Color3.fromRGB(255, 255, 255)
  ClickTP.TextSize = 14.000
  ClickTP.TextXAlignment = Enum.TextXAlignment.Left
  table.insert(shade2,ClickTP)
  table.insert(text1,ClickTP)
  
  Select.Name = "Select"
  Select.Parent = ClickTP
  Select.BackgroundColor3 = Color3.fromRGB(78, 78, 79)
  Select.BorderSizePixel = 0
  Select.Position = UDim2.new(0, 160, 0, 0)
  Select.Size = UDim2.new(0, 40, 0, 20)
  Select.ZIndex = 10
  Select.Font = Enum.Font.SourceSans
  Select.Text = "Add"
  Select.TextColor3 = Color3.fromRGB(0, 0, 0)
  Select.TextSize = 14.000
  table.insert(shade3,Select)
  table.insert(text2,Select)
  
  ClickDelete.Name = "Click Delete (Hold Key & Click)"
  ClickDelete.Parent = Toggles
  ClickDelete.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  ClickDelete.BorderSizePixel = 0
  ClickDelete.Position = UDim2.new(0, 0, 0, 25)
  ClickDelete.Size = UDim2.new(0, 200, 0, 20)
  ClickDelete.ZIndex = 10
  ClickDelete.Font = Enum.Font.SourceSans
  ClickDelete.Text = "    Click Delete (Hold Key & Click)"
  ClickDelete.TextColor3 = Color3.fromRGB(255, 255, 255)
  ClickDelete.TextSize = 14.000
  ClickDelete.TextXAlignment = Enum.TextXAlignment.Left
  table.insert(shade2,ClickDelete)
  table.insert(text1,ClickDelete)
  
  Select_2.Name = "Select"
  Select_2.Parent = ClickDelete
  Select_2.BackgroundColor3 = Color3.fromRGB(78, 78, 79)
  Select_2.BorderSizePixel = 0
  Select_2.Position = UDim2.new(0, 160, 0, 0)
  Select_2.Size = UDim2.new(0, 40, 0, 20)
  Select_2.ZIndex = 10
  Select_2.Font = Enum.Font.SourceSans
  Select_2.Text = "Add"
  Select_2.TextColor3 = Color3.fromRGB(0, 0, 0)
  Select_2.TextSize = 14.000
  table.insert(shade3,Select_2)
  table.insert(text2,Select_2)
  
  Cmdbar_2.Name = "Cmdbar_2"
  Cmdbar_2.Parent = background_2
  Cmdbar_2.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Cmdbar_2.BorderSizePixel = 0
  Cmdbar_2.Position = UDim2.new(0, 150, 0, 35)
  Cmdbar_2.Size = UDim2.new(0, 150, 0, 20)
  Cmdbar_2.ZIndex = 10
  Cmdbar_2.Font = Enum.Font.SourceSans
  Cmdbar_2.PlaceholderText = "Command"
  Cmdbar_2.Text = ""
  Cmdbar_2.TextColor3 = Color3.fromRGB(255, 255, 255)
  Cmdbar_2.TextSize = 14.000
  Cmdbar_2.TextXAlignment = Enum.TextXAlignment.Left
  
  Cmdbar_3.Name = "Cmdbar_3"
  Cmdbar_3.Parent = background_2
  Cmdbar_3.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Cmdbar_3.BorderSizePixel = 0
  Cmdbar_3.Position = UDim2.new(0, 150, 0, 60)
  Cmdbar_3.Size = UDim2.new(0, 150, 0, 20)
  Cmdbar_3.ZIndex = 10
  Cmdbar_3.Font = Enum.Font.SourceSans
  Cmdbar_3.PlaceholderText = "Command 2"
  Cmdbar_3.Text = ""
  Cmdbar_3.TextColor3 = Color3.fromRGB(255, 255, 255)
  Cmdbar_3.TextSize = 14.000
  Cmdbar_3.TextXAlignment = Enum.TextXAlignment.Left
  
  CreateToggle.Name = "CreateToggle"
  CreateToggle.Parent = background_2
  CreateToggle.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  CreateToggle.BackgroundTransparency = 1
  CreateToggle.BorderSizePixel = 0
  CreateToggle.Position = UDim2.new(0, 152, 0, 10)
  CreateToggle.Size = UDim2.new(0, 198, 0, 20)
  CreateToggle.ZIndex = 10
  CreateToggle.Font = Enum.Font.SourceSans
  CreateToggle.Text = "Create Toggle"
  CreateToggle.TextColor3 = Color3.fromRGB(255, 255, 255)
  CreateToggle.TextSize = 14.000
  CreateToggle.TextXAlignment = Enum.TextXAlignment.Left
  table.insert(text1,CreateToggle)
  
  Button_2.Name = "Button"
  Button_2.Parent = CreateToggle
  Button_2.BackgroundColor3 = Color3.fromRGB(78, 78, 79)
  Button_2.BorderSizePixel = 0
  Button_2.Position = UDim2.new(1, -20, 0, 0)
  Button_2.Size = UDim2.new(0, 20, 0, 20)
  Button_2.ZIndex = 10
  table.insert(shade3,Button_2)
  
  On_2.Name = "On"
  On_2.Parent = Button_2
  On_2.BackgroundColor3 = Color3.fromRGB(150, 150, 151)
  On_2.BackgroundTransparency = 1
  On_2.BorderSizePixel = 0
  On_2.Position = UDim2.new(0, 2, 0, 2)
  On_2.Size = UDim2.new(0, 16, 0, 16)
  On_2.ZIndex = 10
  On_2.Font = Enum.Font.SourceSans
  On_2.Text = ""
  On_2.TextColor3 = Color3.fromRGB(0, 0, 0)
  On_2.TextSize = 14.000
  
  shadow_2.Name = "shadow"
  shadow_2.Parent = KeybindEditor
  shadow_2.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  shadow_2.BorderSizePixel = 0
  shadow_2.Size = UDim2.new(0, 360, 0, 20)
  shadow_2.ZIndex = 10
  table.insert(shade2,shadow_2)
  
  PopupText_2.Name = "PopupText_2"
  PopupText_2.Parent = shadow_2
  PopupText_2.BackgroundTransparency = 1
  PopupText_2.Size = UDim2.new(1, 0, 0.949999988, 0)
  PopupText_2.ZIndex = 10
  PopupText_2.Font = Enum.Font.SourceSans
  PopupText_2.Text = "Set Keybinds"
  PopupText_2.TextColor3 = Color3.fromRGB(255, 255, 255)
  PopupText_2.TextSize = 14.000
  PopupText_2.TextWrapped = true
  table.insert(text1,PopupText_2)
  
  Exit_2.Name = "Exit_2"
  Exit_2.Parent = shadow_2
  Exit_2.BackgroundTransparency = 1
  Exit_2.Position = UDim2.new(1, -20, 0, 0)
  Exit_2.Size = UDim2.new(0, 20, 0, 20)
  Exit_2.ZIndex = 10
  Exit_2.Text = ""
  
  ExitImage_2.Parent = Exit_2
  ExitImage_2.BackgroundColor3 = Color3.fromRGB(255, 255, 255)
  ExitImage_2.BackgroundTransparency = 1
  ExitImage_2.Position = UDim2.new(0, 5, 0, 5)
  ExitImage_2.Size = UDim2.new(0, 10, 0, 10)
  ExitImage_2.ZIndex = 10
  ExitImage_2.Image = "rbxassetid://5054663650"
  
  PositionsFrame.Name = "PositionsFrame"
  PositionsFrame.Parent = Settings
  PositionsFrame.Active = true
  PositionsFrame.BackgroundColor3 = Color3.fromRGB(36, 36, 37)
  PositionsFrame.BorderSizePixel = 0
  PositionsFrame.Size = UDim2.new(0, 250, 0, 175)
  PositionsFrame.Position = UDim2.new(0, 0, 0, 175)
  PositionsFrame.ZIndex = 10
  table.insert(shade1,PositionsFrame)
  
  Close_3.Name = "Close"
  Close_3.Parent = PositionsFrame
  Close_3.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Close_3.BorderSizePixel = 0
  Close_3.Position = UDim2.new(0, 205, 0, 150)
  Close_3.Size = UDim2.new(0, 40, 0, 20)
  Close_3.Font = Enum.Font.SourceSans
  Close_3.TextSize = 14
  Close_3.Text = "Close"
  Close_3.TextColor3 = Color3.new(1, 1, 1)
  Close_3.ZIndex = 10
  table.insert(shade2,Close_3)
  table.insert(text1,Close_3)
  
  Delete_5.Name = "Delete"
  Delete_5.Parent = PositionsFrame
  Delete_5.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Delete_5.BorderSizePixel = 0
  Delete_5.Position = UDim2.new(0, 50, 0, 150)
  Delete_5.Size = UDim2.new(0, 40, 0, 20)
  Delete_5.Font = Enum.Font.SourceSans
  Delete_5.TextSize = 14
  Delete_5.Text = "Clear"
  Delete_5.TextColor3 = Color3.new(1, 1, 1)
  Delete_5.ZIndex = 10
  table.insert(shade2,Delete_5)
  table.insert(text1,Delete_5)
  
  Part.Name = "PartGoto"
  Part.Parent = PositionsFrame
  Part.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Part.BorderSizePixel = 0
  Part.Position = UDim2.new(0, 5, 0, 150)
  Part.Size = UDim2.new(0, 40, 0, 20)
  Part.Font = Enum.Font.SourceSans
  Part.TextSize = 14
  Part.Text = "Part"
  Part.TextColor3 = Color3.new(1, 1, 1)
  Part.ZIndex = 10
  table.insert(shade2,Part)
  table.insert(text1,Part)
  
  Holder_4.Name = "Holder"
  Holder_4.Parent = PositionsFrame
  Holder_4.BackgroundTransparency = 1
  Holder_4.BorderSizePixel = 0
  Holder_4.Position = UDim2.new(0, 0, 0, 0)
  Holder_4.Selectable = false
  Holder_4.Size = UDim2.new(0, 250, 0, 145)
  Holder_4.ScrollBarImageColor3 = Color3.fromRGB(78,78,79)
  Holder_4.BottomImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  Holder_4.CanvasSize = UDim2.new(0, 0, 0, 0)
  Holder_4.MidImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  Holder_4.ScrollBarThickness = 0
  Holder_4.TopImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  Holder_4.VerticalScrollBarInset = 'Always'
  Holder_4.ZIndex = 10
  
  Example_4.Name = "Example"
  Example_4.Parent = PositionsFrame
  Example_4.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Example_4.BorderSizePixel = 0
  Example_4.Size = UDim2.new(0, 10, 0, 20)
  Example_4.Visible = false
  Example_4.Position = UDim2.new(0, 0, 0, -5)
  Example_4.ZIndex = 10
  table.insert(shade2,Example_4)
  
  Text_5.Name = "Text"
  Text_5.Parent = Example_4
  Text_5.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Text_5.BorderSizePixel = 0
  Text_5.Position = UDim2.new(0, 10, 0, 0)
  Text_5.Size = UDim2.new(0, 240, 0, 20)
  Text_5.Font = Enum.Font.SourceSans
  Text_5.TextSize = 14
  Text_5.Text = "Position"
  Text_5.TextColor3 = Color3.new(1, 1, 1)
  Text_5.TextXAlignment = Enum.TextXAlignment.Left
  Text_5.ZIndex = 10
  table.insert(shade2,Text_5)
  table.insert(text1,Text_5)
  
  Delete_6.Name = "Delete"
  Delete_6.Parent = Text_5
  Delete_6.BackgroundColor3 = Color3.fromRGB(78, 78, 79)
  Delete_6.BorderSizePixel = 0
  Delete_6.Position = UDim2.new(0, 200, 0, 0)
  Delete_6.Size = UDim2.new(0, 40, 0, 20)
  Delete_6.Font = Enum.Font.SourceSans
  Delete_6.TextSize = 14
  Delete_6.Text = "Delete"
  Delete_6.TextColor3 = Color3.new(0, 0, 0)
  Delete_6.ZIndex = 10
  table.insert(shade3,Delete_6)
  table.insert(text2,Delete_6)
  
  TP.Name = "TP"
  TP.Parent = Text_5
  TP.BackgroundColor3 = Color3.fromRGB(78, 78, 79)
  TP.BorderSizePixel = 0
  TP.Position = UDim2.new(0, 155, 0, 0)
  TP.Size = UDim2.new(0, 40, 0, 20)
  TP.Font = Enum.Font.SourceSans
  TP.TextSize = 14
  TP.Text = "Goto"
  TP.TextColor3 = Color3.new(0, 0, 0)
  TP.ZIndex = 10
  table.insert(shade3,TP)
  table.insert(text2,TP)
  
  AliasesFrame.Name = "AliasesFrame"
  AliasesFrame.Parent = Settings
  AliasesFrame.Active = true
  AliasesFrame.BackgroundColor3 = Color3.fromRGB(36, 36, 37)
  AliasesFrame.BorderSizePixel = 0
  AliasesFrame.Position = UDim2.new(0, 0, 0, 175)
  AliasesFrame.Size = UDim2.new(0, 250, 0, 175)
  AliasesFrame.ZIndex = 10
  table.insert(shade1,AliasesFrame)
  
  Close_2.Name = "Close"
  Close_2.Parent = AliasesFrame
  Close_2.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Close_2.BorderSizePixel = 0
  Close_2.Position = UDim2.new(0, 205, 0, 150)
  Close_2.Size = UDim2.new(0, 40, 0, 20)
  Close_2.Font = Enum.Font.SourceSans
  Close_2.TextSize = 14
  Close_2.Text = "Close"
  Close_2.TextColor3 = Color3.new(1, 1, 1)
  Close_2.ZIndex = 10
  table.insert(shade2,Close_2)
  table.insert(text1,Close_2)
  
  Delete_3.Name = "Delete"
  Delete_3.Parent = AliasesFrame
  Delete_3.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Delete_3.BorderSizePixel = 0
  Delete_3.Position = UDim2.new(0, 5, 0, 150)
  Delete_3.Size = UDim2.new(0, 40, 0, 20)
  Delete_3.Font = Enum.Font.SourceSans
  Delete_3.TextSize = 14
  Delete_3.Text = "Clear"
  Delete_3.TextColor3 = Color3.new(1, 1, 1)
  Delete_3.ZIndex = 10
  table.insert(shade2,Delete_3)
  table.insert(text1,Delete_3)
  
  Holder_3.Name = "Holder"
  Holder_3.Parent = AliasesFrame
  Holder_3.BackgroundTransparency = 1
  Holder_3.BorderSizePixel = 0
  Holder_3.Position = UDim2.new(0, 0, 0, 0)
  Holder_3.Size = UDim2.new(0, 250, 0, 145)
  Holder_3.ScrollBarImageColor3 = Color3.fromRGB(78,78,79)
  Holder_3.BottomImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  Holder_3.CanvasSize = UDim2.new(0, 0, 0, 0)
  Holder_3.MidImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  Holder_3.ScrollBarThickness = 0
  Holder_3.TopImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  Holder_3.VerticalScrollBarInset = 'Always'
  Holder_3.ZIndex = 10
  
  Example_3.Name = "Example"
  Example_3.Parent = AliasesFrame
  Example_3.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Example_3.BorderSizePixel = 0
  Example_3.Size = UDim2.new(0, 10, 0, 20)
  Example_3.Visible = false
  Example_3.ZIndex = 10
  table.insert(shade2,Example_3)
  
  Text_4.Name = "Text"
  Text_4.Parent = Example_3
  Text_4.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Text_4.BorderSizePixel = 0
  Text_4.Position = UDim2.new(0, 10, 0, 0)
  Text_4.Size = UDim2.new(0, 240, 0, 20)
  Text_4.Font = Enum.Font.SourceSans
  Text_4.TextSize = 14
  Text_4.Text = "honk"
  Text_4.TextColor3 = Color3.new(1, 1, 1)
  Text_4.TextXAlignment = Enum.TextXAlignment.Left
  Text_4.ZIndex = 10
  table.insert(shade2,Text_4)
  table.insert(text1,Text_4)
  
  Delete_4.Name = "Delete"
  Delete_4.Parent = Text_4
  Delete_4.BackgroundColor3 = Color3.fromRGB(78, 78, 79)
  Delete_4.BorderSizePixel = 0
  Delete_4.Position = UDim2.new(0, 200, 0, 0)
  Delete_4.Size = UDim2.new(0, 40, 0, 20)
  Delete_4.Font = Enum.Font.SourceSans
  Delete_4.TextSize = 14
  Delete_4.Text = "Delete"
  Delete_4.TextColor3 = Color3.new(0, 0, 0)
  Delete_4.ZIndex = 10
  table.insert(shade3,Delete_4)
  table.insert(text2,Delete_4)
  
  PluginsFrame.Name = "PluginsFrame"
  PluginsFrame.Parent = Settings
  PluginsFrame.Active = true
  PluginsFrame.BackgroundColor3 = Color3.fromRGB(36, 36, 37)
  PluginsFrame.BorderSizePixel = 0
  PluginsFrame.Position = UDim2.new(0, 0, 0, 175)
  PluginsFrame.Size = UDim2.new(0, 250, 0, 175)
  PluginsFrame.ZIndex = 10
  table.insert(shade1,PluginsFrame)
  
  Close_4.Name = "Close"
  Close_4.Parent = PluginsFrame
  Close_4.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Close_4.BorderSizePixel = 0
  Close_4.Position = UDim2.new(0, 205, 0, 150)
  Close_4.Size = UDim2.new(0, 40, 0, 20)
  Close_4.Font = Enum.Font.SourceSans
  Close_4.TextSize = 14
  Close_4.Text = "Close"
  Close_4.TextColor3 = Color3.new(1, 1, 1)
  Close_4.ZIndex = 10
  table.insert(shade2,Close_4)
  table.insert(text1,Close_4)
  
  Add_3.Name = "Add"
  Add_3.Parent = PluginsFrame
  Add_3.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Add_3.BorderSizePixel = 0
  Add_3.Position = UDim2.new(0, 5, 0, 150)
  Add_3.Size = UDim2.new(0, 40, 0, 20)
  Add_3.Font = Enum.Font.SourceSans
  Add_3.TextSize = 14
  Add_3.Text = "Add"
  Add_3.TextColor3 = Color3.new(1, 1, 1)
  Add_3.ZIndex = 10
  table.insert(shade2,Add_3)
  table.insert(text1,Add_3)
  
  Holder_5.Name = "Holder"
  Holder_5.Parent = PluginsFrame
  Holder_5.BackgroundTransparency = 1
  Holder_5.BorderSizePixel = 0
  Holder_5.Position = UDim2.new(0, 0, 0, 0)
  Holder_5.Selectable = false
  Holder_5.Size = UDim2.new(0, 250, 0, 145)
  Holder_5.ScrollBarImageColor3 = Color3.fromRGB(78,78,79)
  Holder_5.BottomImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  Holder_5.CanvasSize = UDim2.new(0, 0, 0, 0)
  Holder_5.MidImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  Holder_5.ScrollBarThickness = 0
  Holder_5.TopImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  Holder_5.VerticalScrollBarInset = 'Always'
  Holder_5.ZIndex = 10
  
  Example_5.Name = "Example"
  Example_5.Parent = PluginsFrame
  Example_5.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Example_5.BorderSizePixel = 0
  Example_5.Size = UDim2.new(0, 10, 0, 20)
  Example_5.Visible = false
  Example_5.ZIndex = 10
  table.insert(shade2,Example_5)
  
  Text_6.Name = "Text"
  Text_6.Parent = Example_5
  Text_6.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Text_6.BorderSizePixel = 0
  Text_6.Position = UDim2.new(0, 10, 0, 0)
  Text_6.Size = UDim2.new(0, 240, 0, 20)
  Text_6.Font = Enum.Font.SourceSans
  Text_6.TextSize = 14
  Text_6.Text = "F4 > Toggle Fly"
  Text_6.TextColor3 = Color3.new(1, 1, 1)
  Text_6.TextXAlignment = Enum.TextXAlignment.Left
  Text_6.ZIndex = 10
  table.insert(shade2,Text_6)
  table.insert(text1,Text_6)
  
  Delete_7.Name = "Delete"
  Delete_7.Parent = Text_6
  Delete_7.BackgroundColor3 = Color3.fromRGB(78, 78, 79)
  Delete_7.BorderSizePixel = 0
  Delete_7.Position = UDim2.new(0, 200, 0, 0)
  Delete_7.Size = UDim2.new(0, 40, 0, 20)
  Delete_7.Font = Enum.Font.SourceSans
  Delete_7.TextSize = 14
  Delete_7.Text = "Delete"
  Delete_7.TextColor3 = Color3.new(0, 0, 0)
  Delete_7.ZIndex = 10
  table.insert(shade3,Delete_7)
  table.insert(text2,Delete_7)
  
  PluginEditor.Name = randomString()
  PluginEditor.Parent = PARENT
  PluginEditor.BorderSizePixel = 0
  PluginEditor.Active = true
  PluginEditor.BackgroundTransparency = 1
  PluginEditor.Position = UDim2.new(0.5, -180, 0, -500)
  PluginEditor.Size = UDim2.new(0, 360, 0, 20)
  PluginEditor.ZIndex = 10
  
  background_3.Name = "background"
  background_3.Parent = PluginEditor
  background_3.Active = true
  background_3.BackgroundColor3 = Color3.fromRGB(36, 36, 37)
  background_3.BorderSizePixel = 0
  background_3.Position = UDim2.new(0, 0, 0, 20)
  background_3.Size = UDim2.new(0, 360, 0, 160)
  background_3.ZIndex = 10
  table.insert(shade1,background_3)
  
  Dark_2.Name = "Dark"
  Dark_2.Parent = background_3
  Dark_2.Active = true
  Dark_2.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  Dark_2.BorderSizePixel = 0
  Dark_2.Position = UDim2.new(0, 222, 0, 0)
  Dark_2.Size = UDim2.new(0, 2, 0, 160)
  Dark_2.ZIndex = 10
  table.insert(shade2,Dark_2)
  
  Img.Name = "Img"
  Img.Parent = background_3
  Img.BackgroundTransparency = 1
  Img.Position = UDim2.new(0, 242, 0, 3)
  Img.Size = UDim2.new(0, 100, 0, 95)
  Img.Image = "rbxassetid://4113050383"
  Img.ZIndex = 10
  
  AddPlugin.Name = "AddPlugin"
  AddPlugin.Parent = background_3
  AddPlugin.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  AddPlugin.BorderSizePixel = 0
  AddPlugin.Position = UDim2.new(0, 235, 0, 100)
  AddPlugin.Size = UDim2.new(0, 115, 0, 50)
  AddPlugin.Font = Enum.Font.SourceSans
  AddPlugin.TextSize = 14
  AddPlugin.Text = "Add Plugin"
  AddPlugin.TextColor3 = Color3.new(1, 1, 1)
  AddPlugin.ZIndex = 10
  table.insert(shade2,AddPlugin)
  table.insert(text1,AddPlugin)
  
  FileName.Name = "FileName"
  FileName.Parent = background_3
  FileName.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  FileName.BorderSizePixel = 0
  FileName.Position = UDim2.new(0.028, 0, 0.625, 0)
  FileName.Size = UDim2.new(0, 200, 0, 50)
  FileName.Font = Enum.Font.SourceSans
  FileName.TextSize = 14
  FileName.Text = "Plugin File Name"
  FileName.TextColor3 = Color3.new(1, 1, 1)
  FileName.ZIndex = 10
  table.insert(shade2,FileName)
  table.insert(text1,FileName)
  
  About.Name = "About"
  About.Parent = background_3
  About.BackgroundTransparency = 1
  About.BorderSizePixel = 0
  About.Position = UDim2.new(0, 17, 0, 10)
  About.Size = UDim2.new(0, 187, 0, 49)
  About.Font = Enum.Font.SourceSans
  About.TextSize = 14
  About.Text = "Plugins are .iy files and should be located in the 'workspace' folder of your exploit."
  About.TextColor3 = Color3.fromRGB(255, 255, 255)
  About.TextWrapped = true
  About.TextYAlignment = Enum.TextYAlignment.Top
  About.ZIndex = 10
  table.insert(text1,About)
  
  Directions_2.Name = "Directions"
  Directions_2.Parent = background_3
  Directions_2.BackgroundTransparency = 1
  Directions_2.BorderSizePixel = 0
  Directions_2.Position = UDim2.new(0, 17, 0, 60)
  Directions_2.Size = UDim2.new(0, 187, 0, 49)
  Directions_2.Font = Enum.Font.SourceSans
  Directions_2.TextSize = 14
  Directions_2.Text = "Type the name of the plugin file you want to add below."
  Directions_2.TextColor3 = Color3.fromRGB(255, 255, 255)
  Directions_2.TextWrapped = true
  Directions_2.TextYAlignment = Enum.TextYAlignment.Top
  Directions_2.ZIndex = 10
  table.insert(text1,Directions_2)
  
  shadow_3.Name = "shadow"
  shadow_3.Parent = PluginEditor
  shadow_3.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  shadow_3.BorderSizePixel = 0
  shadow_3.Size = UDim2.new(0, 360, 0, 20)
  shadow_3.ZIndex = 10
  table.insert(shade2,shadow_3)
  
  PopupText_3.Name = "PopupText"
  PopupText_3.Parent = shadow_3
  PopupText_3.BackgroundTransparency = 1
  PopupText_3.Size = UDim2.new(1, 0, 0.95, 0)
  PopupText_3.ZIndex = 10
  PopupText_3.Font = Enum.Font.SourceSans
  PopupText_3.TextSize = 14
  PopupText_3.Text = "Add Plugins"
  PopupText_3.TextColor3 = Color3.new(1, 1, 1)
  PopupText_3.TextWrapped = true
  table.insert(text1,PopupText_3)
  
  Exit_3.Name = "Exit"
  Exit_3.Parent = shadow_3
  Exit_3.BackgroundTransparency = 1
  Exit_3.Position = UDim2.new(1, -20, 0, 0)
  Exit_3.Size = UDim2.new(0, 20, 0, 20)
  Exit_3.Text = ""
  Exit_3.ZIndex = 10
  
  ExitImage_3.Parent = Exit_3
  ExitImage_3.BackgroundColor3 = Color3.new(1, 1, 1)
  ExitImage_3.BackgroundTransparency = 1
  ExitImage_3.Position = UDim2.new(0, 5, 0, 5)
  ExitImage_3.Size = UDim2.new(0, 10, 0, 10)
  ExitImage_3.Image = "rbxassetid://5054663650"
  ExitImage_3.ZIndex = 10
  
  AliasHint.Name = "AliasHint"
  AliasHint.Parent = AliasesFrame
  AliasHint.BackgroundTransparency = 1
  AliasHint.BorderSizePixel = 0
  AliasHint.Position = UDim2.new(0, 25, 0, 40)
  AliasHint.Size = UDim2.new(0, 200, 0, 50)
  AliasHint.Font = Enum.Font.SourceSansItalic
  AliasHint.TextSize = 16
  AliasHint.Text = "Add aliases by using the 'addalias' command"
  AliasHint.TextColor3 = Color3.new(1, 1, 1)
  AliasHint.TextStrokeColor3 = Color3.new(1, 1, 1)
  AliasHint.TextWrapped = true
  AliasHint.ZIndex = 10
  table.insert(text1,AliasHint)
  
  PluginsHint.Name = "PluginsHint"
  PluginsHint.Parent = PluginsFrame
  PluginsHint.BackgroundTransparency = 1
  PluginsHint.BorderSizePixel = 0
  PluginsHint.Position = UDim2.new(0, 25, 0, 40)
  PluginsHint.Size = UDim2.new(0, 200, 0, 50)
  PluginsHint.Font = Enum.Font.SourceSansItalic
  PluginsHint.TextSize = 16
  PluginsHint.Text = "Download plugins from the IY Discord (discord.gg/78ZuWSq)"
  PluginsHint.TextColor3 = Color3.new(1, 1, 1)
  PluginsHint.TextStrokeColor3 = Color3.new(1, 1, 1)
  PluginsHint.TextWrapped = true
  PluginsHint.ZIndex = 10
  table.insert(text1,PluginsHint)
  
  PositionsHint.Name = "PositionsHint"
  PositionsHint.Parent = PositionsFrame
  PositionsHint.BackgroundTransparency = 1
  PositionsHint.BorderSizePixel = 0
  PositionsHint.Position = UDim2.new(0, 25, 0, 40)
  PositionsHint.Size = UDim2.new(0, 200, 0, 70)
  PositionsHint.Font = Enum.Font.SourceSansItalic
  PositionsHint.TextSize = 16
  PositionsHint.Text = "Use the 'swp' or 'setwaypoint' command to add a position using your character (NOTE: Part teleports will not save)"
  PositionsHint.TextColor3 = Color3.new(1, 1, 1)
  PositionsHint.TextStrokeColor3 = Color3.new(1, 1, 1)
  PositionsHint.TextWrapped = true
  PositionsHint.ZIndex = 10
  table.insert(text1,PositionsHint)
  
  ToPartFrame.Name = randomString()
  ToPartFrame.Parent = PARENT
  ToPartFrame.Active = true
  ToPartFrame.BackgroundTransparency = 1
  ToPartFrame.Position = UDim2.new(0.5, -180, 0, -500)
  ToPartFrame.Size = UDim2.new(0, 360, 0, 20)
  ToPartFrame.ZIndex = 10
  
  background_4.Name = "background"
  background_4.Parent = ToPartFrame
  background_4.Active = true
  background_4.BackgroundColor3 = Color3.fromRGB(36, 36, 37)
  background_4.BorderSizePixel = 0
  background_4.Position = UDim2.new(0, 0, 0, 20)
  background_4.Size = UDim2.new(0, 360, 0, 117)
  background_4.ZIndex = 10
  table.insert(shade1,background_4)
  
  ChoosePart.Name = "ChoosePart"
  ChoosePart.Parent = background_4
  ChoosePart.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  ChoosePart.BorderSizePixel = 0
  ChoosePart.Position = UDim2.new(0, 100, 0, 55)
  ChoosePart.Size = UDim2.new(0, 75, 0, 30)
  ChoosePart.Font = Enum.Font.SourceSans
  ChoosePart.TextSize = 14
  ChoosePart.Text = "Select Part"
  ChoosePart.TextColor3 = Color3.new(1, 1, 1)
  ChoosePart.ZIndex = 10
  table.insert(shade2,ChoosePart)
  table.insert(text1,ChoosePart)
  
  CopyPath.Name = "CopyPath"
  CopyPath.Parent = background_4
  CopyPath.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  CopyPath.BorderSizePixel = 0
  CopyPath.Position = UDim2.new(0, 185, 0, 55)
  CopyPath.Size = UDim2.new(0, 75, 0, 30)
  CopyPath.Font = Enum.Font.SourceSans
  CopyPath.TextSize = 14
  CopyPath.Text = "Copy Path"
  CopyPath.TextColor3 = Color3.new(1, 1, 1)
  CopyPath.ZIndex = 10
  table.insert(shade2,CopyPath)
  table.insert(text1,CopyPath)
  
  Directions_3.Name = "Directions"
  Directions_3.Parent = background_4
  Directions_3.BackgroundTransparency = 1
  Directions_3.BorderSizePixel = 0
  Directions_3.Position = UDim2.new(0, 51, 0, 17)
  Directions_3.Size = UDim2.new(0, 257, 0, 32)
  Directions_3.Font = Enum.Font.SourceSans
  Directions_3.TextSize = 14
  Directions_3.Text = 'Click on a part and then click the "Select Part" button below to set it as a teleport location'
  Directions_3.TextColor3 = Color3.new(1, 1, 1)
  Directions_3.TextWrapped = true
  Directions_3.TextYAlignment = Enum.TextYAlignment.Top
  Directions_3.ZIndex = 10
  table.insert(text1,Directions_3)
  
  Path.Name = "Path"
  Path.Parent = background_4
  Path.BackgroundTransparency = 1
  Path.BorderSizePixel = 0
  Path.Position = UDim2.new(0, 0, 0, 94)
  Path.Size = UDim2.new(0, 360, 0, 16)
  Path.Font = Enum.Font.SourceSansItalic
  Path.TextSize = 14
  Path.Text = ""
  Path.TextColor3 = Color3.new(1, 1, 1)
  Path.TextScaled = true
  Path.TextWrapped = true
  Path.TextYAlignment = Enum.TextYAlignment.Top
  Path.ZIndex = 10
  table.insert(text1,Path)
  
  shadow_4.Name = "shadow"
  shadow_4.Parent = ToPartFrame
  shadow_4.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
  shadow_4.BorderSizePixel = 0
  shadow_4.Size = UDim2.new(0, 360, 0, 20)
  shadow_4.ZIndex = 10
  table.insert(shade2,shadow_4)
  
  PopupText_5.Name = "PopupText"
  PopupText_5.Parent = shadow_4
  PopupText_5.BackgroundTransparency = 1
  PopupText_5.Size = UDim2.new(1, 0, 0.95, 0)
  PopupText_5.ZIndex = 10
  PopupText_5.Font = Enum.Font.SourceSans
  PopupText_5.TextSize = 14
  PopupText_5.Text = "Teleport to Part"
  PopupText_5.TextColor3 = Color3.new(1, 1, 1)
  PopupText_5.TextWrapped = true
  table.insert(text1,PopupText_5)
  
  Exit_4.Name = "Exit"
  Exit_4.Parent = shadow_4
  Exit_4.BackgroundTransparency = 1
  Exit_4.Position = UDim2.new(1, -20, 0, 0)
  Exit_4.Size = UDim2.new(0, 20, 0, 20)
  Exit_4.Text = ""
  Exit_4.ZIndex = 10
  
  ExitImage_5.Parent = Exit_4
  ExitImage_5.BackgroundColor3 = Color3.new(1, 1, 1)
  ExitImage_5.BackgroundTransparency = 1
  ExitImage_5.Position = UDim2.new(0, 5, 0, 5)
  ExitImage_5.Size = UDim2.new(0, 10, 0, 10)
  ExitImage_5.Image = "rbxassetid://5054663650"
  ExitImage_5.ZIndex = 10
  
  logs.Name = randomString()
  logs.Parent = PARENT
  logs.Active = true
  logs.BackgroundTransparency = 1
  logs.Position = UDim2.new(0, 0, 1, 10)
  logs.Size = UDim2.new(0, 338, 0, 20)
  logs.ZIndex = 10
  
  shadow.Name = "shadow"
  shadow.Parent = logs
  shadow.BackgroundColor3 = Color3.new(0.180392, 0.180392, 0.184314)
  shadow.BorderSizePixel = 0
  shadow.Position = UDim2.new(0, 0, 0.00999999978, 0)
  shadow.Size = UDim2.new(0, 338, 0, 20)
  shadow.ZIndex = 10
  table.insert(shade2,shadow)
  
  Hide.Name = "Hide"
  Hide.Parent = shadow
  Hide.BackgroundTransparency = 1
  Hide.Position = UDim2.new(1, -40, 0, 0)
  Hide.Size = UDim2.new(0, 20, 0, 20)
  Hide.ZIndex = 10
  Hide.Text = ""
  
  ImageLabel.Parent = Hide
  ImageLabel.BackgroundColor3 = Color3.new(1, 1, 1)
  ImageLabel.BackgroundTransparency = 1
  ImageLabel.Position = UDim2.new(0, 3, 0, 3)
  ImageLabel.Size = UDim2.new(0, 14, 0, 14)
  ImageLabel.Image = "rbxassetid://2406617031"
  ImageLabel.ZIndex = 10
  
  PopupText.Name = "PopupText"
  PopupText.Parent = shadow
  PopupText.BackgroundTransparency = 1
  PopupText.Size = UDim2.new(1, 0, 0.949999988, 0)
  PopupText.ZIndex = 10
  PopupText.Font = Enum.Font.SourceSans
  PopupText.FontSize = Enum.FontSize.Size14
  PopupText.Text = "Logs"
  PopupText.TextColor3 = Color3.new(1, 1, 1)
  PopupText.TextWrapped = true
  table.insert(text1,PopupText)
  
  Exit.Name = "Exit"
  Exit.Parent = shadow
  Exit.BackgroundTransparency = 1
  Exit.Position = UDim2.new(1, -20, 0, 0)
  Exit.Size = UDim2.new(0, 20, 0, 20)
  Exit.ZIndex = 10
  Exit.Text = ""
  
  ImageLabel_2.Parent = Exit
  ImageLabel_2.BackgroundColor3 = Color3.new(1, 1, 1)
  ImageLabel_2.BackgroundTransparency = 1
  ImageLabel_2.Position = UDim2.new(0, 5, 0, 5)
  ImageLabel_2.Size = UDim2.new(0, 10, 0, 10)
  ImageLabel_2.Image = "rbxassetid://5054663650"
  ImageLabel_2.ZIndex = 10
  
  background.Name = "background"
  background.Parent = logs
  background.Active = true
  background.BackgroundColor3 = Color3.new(0.141176, 0.141176, 0.145098)
  background.BorderSizePixel = 0
  background.ClipsDescendants = true
  background.Position = UDim2.new(0, 0, 1, 0)
  background.Size = UDim2.new(0, 338, 0, 245)
  background.ZIndex = 10
  
  chat.Name = "chat"
  chat.Parent = background
  chat.Active = true
  chat.BackgroundColor3 = Color3.new(0.141176, 0.141176, 0.145098)
  chat.BorderSizePixel = 0
  chat.ClipsDescendants = true
  chat.Size = UDim2.new(0, 338, 0, 245)
  chat.ZIndex = 10
  table.insert(shade1,chat)
  
  Clear.Name = "Clear"
  Clear.Parent = chat
  Clear.BackgroundColor3 = Color3.new(0.180392, 0.180392, 0.184314)
  Clear.BorderSizePixel = 0
  Clear.Position = UDim2.new(0, 5, 0, 220)
  Clear.Size = UDim2.new(0, 50, 0, 20)
  Clear.ZIndex = 10
  Clear.Font = Enum.Font.SourceSans
  Clear.FontSize = Enum.FontSize.Size14
  Clear.Text = "Clear"
  Clear.TextColor3 = Color3.new(1, 1, 1)
  table.insert(shade2,Clear)
  table.insert(text1,Clear)
  
  SaveChatlogs.Name = "SaveChatlogs"
  SaveChatlogs.Parent = chat
  SaveChatlogs.BackgroundColor3 = Color3.new(0.180392, 0.180392, 0.184314)
  SaveChatlogs.BorderSizePixel = 0
  SaveChatlogs.Position = UDim2.new(0, 258, 0, 220)
  SaveChatlogs.Size = UDim2.new(0, 75, 0, 20)
  SaveChatlogs.ZIndex = 10
  SaveChatlogs.Font = Enum.Font.SourceSans
  SaveChatlogs.FontSize = Enum.FontSize.Size14
  SaveChatlogs.Text = "Save To .txt"
  SaveChatlogs.TextColor3 = Color3.new(1, 1, 1)
  table.insert(shade2,SaveChatlogs)
  table.insert(text1,SaveChatlogs)
  
  Toggle.Name = "Toggle"
  Toggle.Parent = chat
  Toggle.BackgroundColor3 = Color3.new(0.180392, 0.180392, 0.184314)
  Toggle.BorderSizePixel = 0
  Toggle.Position = UDim2.new(0, 60, 0, 220)
  Toggle.Size = UDim2.new(0, 66, 0, 20)
  Toggle.ZIndex = 10
  Toggle.Font = Enum.Font.SourceSans
  Toggle.FontSize = Enum.FontSize.Size14
  Toggle.Text = "Disabled"
  Toggle.TextColor3 = Color3.new(1, 1, 1)
  table.insert(shade2,Toggle)
  table.insert(text1,Toggle)
  
  scroll_2.Name = "scroll"
  scroll_2.Parent = chat
  scroll_2.BackgroundColor3 = Color3.new(0.180392, 0.180392, 0.184314)
  scroll_2.BorderSizePixel = 0
  scroll_2.Position = UDim2.new(0, 5, 0, 25)
  scroll_2.Size = UDim2.new(0, 328, 0, 190)
  scroll_2.ZIndex = 10
  scroll_2.BottomImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  scroll_2.CanvasSize = UDim2.new(0, 0, 0, 10)
  scroll_2.ScrollBarThickness = 8
  scroll_2.TopImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  table.insert(scroll,scroll_2)
  table.insert(shade2,scroll_2)
  
  join.Name = "join"
  join.Parent = background
  join.Active = true
  join.BackgroundColor3 = Color3.new(0.141176, 0.141176, 0.145098)
  join.BorderSizePixel = 0
  join.ClipsDescendants = true
  join.Size = UDim2.new(0, 338, 0, 245)
  join.Visible = false
  join.ZIndex = 10
  table.insert(shade1,join)
  
  Toggle_2.Name = "Toggle"
  Toggle_2.Parent = join
  Toggle_2.BackgroundColor3 = Color3.new(0.180392, 0.180392, 0.184314)
  Toggle_2.BorderSizePixel = 0
  Toggle_2.Position = UDim2.new(0, 60, 0, 220)
  Toggle_2.Size = UDim2.new(0, 66, 0, 20)
  Toggle_2.ZIndex = 10
  Toggle_2.Font = Enum.Font.SourceSans
  Toggle_2.FontSize = Enum.FontSize.Size14
  Toggle_2.Text = "Disabled"
  Toggle_2.TextColor3 = Color3.new(1, 1, 1)
  table.insert(shade2,Toggle_2)
  table.insert(text1,Toggle_2)
  
  Clear_2.Name = "Clear"
  Clear_2.Parent = join
  Clear_2.BackgroundColor3 = Color3.new(0.180392, 0.180392, 0.184314)
  Clear_2.BorderSizePixel = 0
  Clear_2.Position = UDim2.new(0, 5, 0, 220)
  Clear_2.Size = UDim2.new(0, 50, 0, 20)
  Clear_2.ZIndex = 10
  Clear_2.Font = Enum.Font.SourceSans
  Clear_2.FontSize = Enum.FontSize.Size14
  Clear_2.Text = "Clear"
  Clear_2.TextColor3 = Color3.new(1, 1, 1)
  table.insert(shade2,Clear_2)
  table.insert(text1,Clear_2)
  
  scroll_3.Name = "scroll"
  scroll_3.Parent = join
  scroll_3.BackgroundColor3 = Color3.new(0.180392, 0.180392, 0.184314)
  scroll_3.BorderSizePixel = 0
  scroll_3.Position = UDim2.new(0, 5, 0, 25)
  scroll_3.Size = UDim2.new(0, 328, 0, 190)
  scroll_3.ZIndex = 10
  scroll_3.BottomImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  scroll_3.CanvasSize = UDim2.new(0, 0, 0, 10)
  scroll_3.ScrollBarThickness = 8
  scroll_3.TopImage = "rbxasset://textures/ui/Scroll/scroll-middle.png"
  table.insert(scroll,scroll_3)
  table.insert(shade2,scroll_3)
  
  selectChat.Name = "selectChat"
  selectChat.Parent = background
  selectChat.BackgroundColor3 = Color3.new(0.180392, 0.180392, 0.184314)
  selectChat.BorderSizePixel = 0
  selectChat.Position = UDim2.new(0, 5, 0, 5)
  selectChat.Size = UDim2.new(0, 164, 0, 20)
  selectChat.ZIndex = 10
  selectChat.Font = Enum.Font.SourceSans
  selectChat.FontSize = Enum.FontSize.Size14
  selectChat.Text = "Chat Logs"
  selectChat.TextColor3 = Color3.new(1, 1, 1)
  table.insert(shade2,selectChat)
  table.insert(text1,selectChat)
  
  selectJoin.Name = "selectJoin"
  selectJoin.Parent = background
  selectJoin.BackgroundColor3 = Color3.new(0.305882, 0.305882, 0.309804)
  selectJoin.BorderSizePixel = 0
  selectJoin.Position = UDim2.new(0, 169, 0, 5)
  selectJoin.Size = UDim2.new(0, 164, 0, 20)
  selectJoin.ZIndex = 10
  selectJoin.Font = Enum.Font.SourceSans
  selectJoin.FontSize = Enum.FontSize.Size14
  selectJoin.Text = "Join Logs"
  selectJoin.TextColor3 = Color3.new(1, 1, 1)
  table.insert(shade3,selectJoin)
  table.insert(text1,selectJoin)
  
  function create(data)
    local insts = {}
    for i,v in pairs(data) do insts[v[1]] = Instance.new(v[2]) end
  
    for _,v in pairs(data) do
      for prop,val in pairs(v[3]) do
        if type(val) == "table" then
          insts[v[1]][prop] = insts[val[1]]
        else
          insts[v[1]][prop] = val
        end
      end
    end
  
    return insts[1]
  end
  
  TextService = cloneref(game:GetService("TextService"))
  ViewportTextBox = (function()
  
    local funcs = {}
    funcs.Update = function(self)
      local cursorPos = self.TextBox.CursorPosition
      local text = self.TextBox.Text
      if text == "" then self.TextBox.Position = UDim2.new(0,2,0,0) return end
      if cursorPos == -1 then return end
  
      local cursorText = text:sub(1,cursorPos-1)
      local pos = nil
      local leftEnd = -self.TextBox.Position.X.Offset
      local rightEnd = leftEnd + self.View.AbsoluteSize.X
  
      local totalTextSize = TextService:GetTextSize(text,self.TextBox.TextSize,self.TextBox.Font,Vector2.new(999999999,100)).X
      local cursorTextSize = TextService:GetTextSize(cursorText,self.TextBox.TextSize,self.TextBox.Font,Vector2.new(999999999,100)).X
  
      if cursorTextSize > rightEnd then
        pos = math.max(-2,cursorTextSize - self.View.AbsoluteSize.X + 2)
      elseif cursorTextSize < leftEnd then
        pos = math.max(-2,cursorTextSize-2)
      elseif totalTextSize < rightEnd then
        pos = math.max(-2,totalTextSize - self.View.AbsoluteSize.X + 2)
      end
  
      if pos then
        self.TextBox.Position = UDim2.new(0,-pos,0,0)
        self.TextBox.Size = UDim2.new(1,pos,1,0)
      end
    end
  
    local mt = {}
    mt.__index = funcs
  
    local function convert(textbox)
      local obj = setmetatable({OffsetX = 0, TextBox = textbox},mt)
  
      local view = Instance.new("Frame")
      view.BackgroundTransparency = textbox.BackgroundTransparency
      view.BackgroundColor3 = textbox.BackgroundColor3
      view.BorderSizePixel = textbox.BorderSizePixel
      view.BorderColor3 = textbox.BorderColor3
      view.Position = textbox.Position
      view.Size = textbox.Size
      view.ClipsDescendants = true
      view.Name = textbox.Name
      view.ZIndex = 10
      textbox.BackgroundTransparency = 1
      textbox.Position = UDim2.new(0,4,0,0)
      textbox.Size = UDim2.new(1,-8,1,0)
      textbox.TextXAlignment = Enum.TextXAlignment.Left
      textbox.Name = "Input"
      table.insert(text1,textbox)
      table.insert(shade2,view)
  
      obj.View = view
  
      textbox.Changed:Connect(function(prop)
        if prop == "Text" or prop == "CursorPosition" or prop == "AbsoluteSize" then
          obj:Update()
        end
      end)
  
      obj:Update()
  
      view.Parent = textbox.Parent
      textbox.Parent = view
  
      return obj
    end
  
    return {convert = convert}
  end)()
  
  ViewportTextBox.convert(Cmdbar).View.ZIndex = 10
  ViewportTextBox.convert(Cmdbar_2).View.ZIndex = 10
  ViewportTextBox.convert(Cmdbar_3).View.ZIndex = 10
  
  IYMouse = Players.LocalPlayer:GetMouse()
  PlayerGui = Players.LocalPlayer:FindFirstChildWhichIsA("PlayerGui")
  UserInputService = cloneref(game:GetService("UserInputService"))
  TweenService = cloneref(game:GetService("TweenService"))
  HttpService = cloneref(game:GetService("HttpService"))
  MarketplaceService = cloneref(game:GetService("MarketplaceService"))
  RunService = cloneref(game:GetService("RunService"))
  TeleportService = cloneref(game:GetService("TeleportService"))
  StarterGui = cloneref(game:GetService("StarterGui"))
  GuiService = cloneref(game:GetService("GuiService"))
  Lighting = cloneref(game:GetService("Lighting"))
  ContextActionService = cloneref(game:GetService("ContextActionService"))
  NetworkClient = cloneref(game:GetService("NetworkClient"))
  ReplicatedStorage = cloneref(game:GetService("ReplicatedStorage"))
  GroupService = cloneref(game:GetService("GroupService"))
  PathService = cloneref(game:GetService("PathfindingService"))
  SoundService = cloneref(game:GetService("SoundService"))
  Teams = cloneref(game:GetService("Teams"))
  StarterPlayer = cloneref(game:GetService("StarterPlayer"))
  InsertService = cloneref(game:GetService("InsertService"))
  ChatService = cloneref(game:GetService("Chat"))
  ProximityPromptService = cloneref(game:GetService("ProximityPromptService"))
  StatsService = cloneref(game:GetService("Stats"))
  MaterialService = cloneref(game:GetService("MaterialService"))
  AvatarEditorService = cloneref(game:GetService("AvatarEditorService"))
  TextChatService = cloneref(game:GetService("TextChatService"))
  
  sethidden = sethiddenproperty or set_hidden_property or set_hidden_prop
  gethidden = gethiddenproperty or get_hidden_property or get_hidden_prop
  queueteleport = (syn and syn.queue_on_teleport) or queue_on_teleport or (fluxus and fluxus.queue_on_teleport)
  httprequest = (syn and syn.request) or (http and http.request) or http_request or (fluxus and fluxus.request) or request
  PlaceId, JobId = game.PlaceId, game.JobId
  local IsOnMobile = table.find({Enum.Platform.IOS, Enum.Platform.Android}, UserInputService:GetPlatform())
  everyClipboard = setclipboard or toclipboard or set_clipboard or (Clipboard and Clipboard.set)
  
  function writefileExploit()
    if writefile then
      return true
    end
  end
  
  function isNumber(str)
    if tonumber(str) ~= nil or str == 'inf' then
      return true
    end
  end
  
  function getRoot(char)
    local rootPart = char:FindFirstChild('HumanoidRootPart') or char:FindFirstChild('Torso') or char:FindFirstChild('UpperTorso')
    return rootPart
  end
  
  function tools(plr)
    if plr:FindFirstChildOfClass("Backpack"):FindFirstChildOfClass('Tool') or plr.Character:FindFirstChildOfClass('Tool') then
      return true
    end
  end
  
  function r15(plr)
    if plr.Character:FindFirstChildOfClass('Humanoid').RigType == Enum.HumanoidRigType.R15 then
      return true
    end
  end
  
  function toClipboard(txt)
      if everyClipboard then
          everyClipboard(tostring(txt))
          notify("Clipboard", "Copied to clipboard")
      else
          notify("Clipboard", "Your exploit doesn't have the ability to use the clipboard")
      end
  end
  
  function chatMessage(str)
      str = tostring(str)
      if TextChatService.ChatVersion == Enum.ChatVersion.TextChatService then
          TextChatService.TextChannels.RBXGeneral:SendAsync(str)
      else
          ReplicatedStorage.DefaultChatSystemChatEvents.SayMessageRequest:FireServer(str, "All")
      end
  end
  
  function getHierarchy(obj)
    local fullname
    local period
  
    if string.find(obj.Name,' ') then
      fullname = '["'..obj.Name..'"]'
      period = false
    else
      fullname = obj.Name
      period = true
    end
  
    local getS = obj
    local parent = obj
    local service = ''
  
    if getS.Parent ~= game then
      repeat
        getS = getS.Parent
        service = getS.ClassName
      until getS.Parent == game
    end
  
    if parent.Parent ~= getS then
      repeat
        parent = parent.Parent
        if string.find(tostring(parent),' ') then
          if period then
            fullname = '["'..parent.Name..'"].'..fullname
          else
            fullname = '["'..parent.Name..'"]'..fullname
          end
          period = false
        else
          if period then
            fullname = parent.Name..'.'..fullname
          else
            fullname = parent.Name..''..fullname
          end
          period = true
        end
      until parent.Parent == getS
    elseif string.find(tostring(parent),' ') then
      fullname = '["'..parent.Name..'"]'
      period = false
    end
  
    if period then
      return 'game:GetService("'..service..'").'..fullname
    else
      return 'game:GetService("'..service..'")'..fullname
    end
  end
  
  AllWaypoints = {}
  
  local cooldown = false
  function writefileCooldown(name,data)
    task.spawn(function()
      if not cooldown then
        cooldown = true
        writefile(name, data)
      else
        repeat wait() until cooldown == false
        writefileCooldown(name,data)
      end
      wait(3)
      cooldown = false
    end)
  end
  
  function dragGUI(gui)
    task.spawn(function()
      local dragging
      local dragInput
      local dragStart = Vector3.new(0,0,0)
      local startPos
      local function update(input)
        local delta = input.Position - dragStart
        local Position = UDim2.new(startPos.X.Scale, startPos.X.Offset + delta.X, startPos.Y.Scale, startPos.Y.Offset + delta.Y)
        TweenService:Create(gui, TweenInfo.new(.20), {Position = Position}):Play()
      end
      gui.InputBegan:Connect(function(input)
        if input.UserInputType == Enum.UserInputType.MouseButton1 or input.UserInputType == Enum.UserInputType.Touch then
          dragging = true
          dragStart = input.Position
          startPos = gui.Position
  
          input.Changed:Connect(function()
            if input.UserInputState == Enum.UserInputState.End then
              dragging = false
            end
          end)
        end
      end)
      gui.InputChanged:Connect(function(input)
        if input.UserInputType == Enum.UserInputType.MouseMovement or input.UserInputType == Enum.UserInputType.Touch then
          dragInput = input
        end
      end)
      UserInputService.InputChanged:Connect(function(input)
        if input == dragInput and dragging then
          update(input)
        end
      end)
    end)
  end
  
  dragGUI(logs)
  dragGUI(KeybindEditor)
  dragGUI(PluginEditor)
  dragGUI(ToPartFrame)
  
  eventEditor = (function()
    local events = {}
  
    local function registerEvent(name,sets)
      events[name] = {
        commands = {},
        sets = sets or {}
      }
    end
  
    local onEdited = nil
  
    local function fireEvent(name,...)
      local args = {...}
      local event = events[name]
      if event then
        for i,cmd in pairs(event.commands) do
          local metCondition = true
          for idx,set in pairs(event.sets) do
            local argVal = args[idx]
            local cmdSet = cmd[2][idx]
            local condType = set.Type
            if condType == "Player" then
              if cmdSet == 0 then
                metCondition = metCondition and (tostring(Players.LocalPlayer) == argVal)
              elseif cmdSet ~= 1 then
                metCondition = metCondition and table.find(getPlayer(cmdSet,Players.LocalPlayer),argVal)
              end
            elseif condType == "String" then
              if cmdSet ~= 0 then
                metCondition = metCondition and string.find(argVal:lower(),cmdSet:lower())
              end
            elseif condType == "Number" then
              if cmdSet ~= 0 then
                metCondition = metCondition and tonumber(argVal)<=tonumber(cmdSet)
              end
            end
            if not metCondition then break end
          end
  
          if metCondition then
            pcall(task.spawn(function()
              local cmdStr = cmd[1]
              for count,arg in pairs(args) do
                cmdStr = cmdStr:gsub("%$"..count,arg)
              end
              wait(cmd[3] or 0)
              execCmd(cmdStr)
            end))
          end
        end
      end
    end
  
    local main = create({
      {1,"Frame",{BackgroundColor3=Color3.new(0.14117647707462,0.14117647707462,0.14509804546833),BackgroundTransparency=1,BorderSizePixel=0,Name="EventEditor",Position=UDim2.new(0.5,-175,0,-500),Size=UDim2.new(0,350,0,20),ZIndex=10,}},
      {2,"Frame",{BackgroundColor3=currentShade2,BorderSizePixel=0,Name="TopBar",Parent={1},Size=UDim2.new(1,0,0,20),ZIndex=10,}},
      {3,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Title",Parent={2},Position=UDim2.new(0,0,0,0),Size=UDim2.new(1,0,0.95,0),Text="Event Editor",TextColor3=Color3.new(1,1,1),TextSize=14,TextXAlignment=Enum.TextXAlignment.Center,ZIndex=10,}},
      {4,"TextButton",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Close",Parent={2},Position=UDim2.new(1,-20,0,0),Size=UDim2.new(0,20,0,20),Text="",TextColor3=Color3.new(1,1,1),TextSize=14,ZIndex=10,}},
      {5,"ImageLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Image="rbxassetid://5054663650",Parent={4},Position=UDim2.new(0,5,0,5),Size=UDim2.new(0,10,0,10),ZIndex=10,}},
      {6,"Frame",{BackgroundColor3=currentShade1,BorderSizePixel=0,Name="Content",Parent={1},Position=UDim2.new(0,0,0,20),Size=UDim2.new(1,0,0,202),ZIndex=10,}},
      {7,"ScrollingFrame",{BackgroundColor3=Color3.new(0.14117647707462,0.14117647707462,0.14509804546833),BackgroundTransparency=1,BorderColor3=Color3.new(0.15686275064945,0.15686275064945,0.15686275064945),BorderSizePixel=0,BottomImage="rbxasset://textures/ui/Scroll/scroll-middle.png",CanvasSize=UDim2.new(0,0,0,100),Name="List",Parent={6},Position=UDim2.new(0,5,0,5),ScrollBarImageColor3=Color3.new(0.30588236451149,0.30588236451149,0.3098039329052),ScrollBarThickness=8,Size=UDim2.new(1,-10,1,-10),TopImage="rbxasset://textures/ui/Scroll/scroll-middle.png",ZIndex=10,}},
      {8,"Frame",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Name="Holder",Parent={7},Size=UDim2.new(1,0,1,0),ZIndex=10,}},
      {9,"UIListLayout",{Parent={8},SortOrder=2,}},
      {10,"Frame",{BackgroundColor3=Color3.new(0.14117647707462,0.14117647707462,0.14509804546833),BackgroundTransparency=1,BorderColor3=Color3.new(0.3137255012989,0.3137255012989,0.3137255012989),BorderSizePixel=0,ClipsDescendants=true,Name="Settings",Parent={6},Position=UDim2.new(1,0,0,0),Size=UDim2.new(0,150,1,0),ZIndex=10,}},
      {11,"Frame",{BackgroundColor3=Color3.new(0.14117647707462,0.14117647707462,0.14509804546833),Name="Slider",Parent={10},Position=UDim2.new(0,-150,0,0),Size=UDim2.new(1,0,1,0),ZIndex=10,}},
      {12,"Frame",{BackgroundColor3=Color3.new(0.23529413342476,0.23529413342476,0.23529413342476),BorderColor3=Color3.new(0.3137255012989,0.3137255012989,0.3137255012989),BorderSizePixel=0,Name="Line",Parent={11},Size=UDim2.new(0,1,1,0),ZIndex=10,}},
      {13,"ScrollingFrame",{BackgroundColor3=Color3.new(0.14117647707462,0.14117647707462,0.14509804546833),BackgroundTransparency=1,BorderColor3=Color3.new(0.15686275064945,0.15686275064945,0.15686275064945),BorderSizePixel=0,BottomImage="rbxasset://textures/ui/Scroll/scroll-middle.png",CanvasSize=UDim2.new(0,0,0,100),Name="List",Parent={11},Position=UDim2.new(0,0,0,25),ScrollBarImageColor3=Color3.new(0.30588236451149,0.30588236451149,0.3098039329052),ScrollBarThickness=8,Size=UDim2.new(1,0,1,-25),TopImage="rbxasset://textures/ui/Scroll/scroll-middle.png",ZIndex=10,}},
      {14,"Frame",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Name="Holder",Parent={13},Size=UDim2.new(1,0,1,0),ZIndex=10,}},
      {15,"UIListLayout",{Parent={14},SortOrder=2,}},
      {16,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Title",Parent={11},Size=UDim2.new(1,0,0,20),Text="Event Settings",TextColor3=Color3.new(1,1,1),TextSize=14,ZIndex=10,}},
      {17,"TextButton",{BackgroundColor3=Color3.new(0.14117647707462,0.14117647707462,0.14509804546833),BorderColor3=Color3.new(0.15686275064945,0.15686275064945,0.15686275064945),Font=3,Name="Close",BorderSizePixel=0,Parent={11},Position=UDim2.new(1,-20,0,0),Size=UDim2.new(0,20,0,20),Text="<",TextColor3=Color3.new(1,1,1),TextSize=18,ZIndex=10,}},
      {18,"Folder",{Name="Templates",Parent={10},}},
      {19,"Frame",{BackgroundColor3=Color3.new(0.19607844948769,0.19607844948769,0.19607844948769),BackgroundTransparency=1,BorderColor3=Color3.new(0.15686275064945,0.15686275064945,0.15686275064945),Name="Players",Parent={18},Position=UDim2.new(0,0,0,25),Size=UDim2.new(1,0,0,86),Visible=false,ZIndex=10,}},
      {20,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Title",Parent={19},Size=UDim2.new(1,0,0,20),Text="Choose Players",TextColor3=Color3.new(1,1,1),TextSize=14,ZIndex=10,}},
      {21,"TextLabel",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Font=3,Name="Any",Parent={19},Position=UDim2.new(0,5,0,42),Size=UDim2.new(1,-10,0,20),Text="Any Player",TextColor3=Color3.new(1,1,1),TextSize=14,TextXAlignment=0,ZIndex=10,}},
      {22,"Frame",{BackgroundColor3=Color3.new(0.30588236451149,0.30588236451149,0.3098039329052),BorderSizePixel=0,Name="Button",Parent={21},Position=UDim2.new(1,-20,0,0),Size=UDim2.new(0,20,0,20),ZIndex=10,}},
      {23,"TextButton",{BackgroundColor3=Color3.new(0.58823531866074,0.58823531866074,0.59215688705444),BackgroundTransparency=1,BorderSizePixel=0,Font=3,Name="On",Parent={22},Position=UDim2.new(0,2,0,2),Size=UDim2.new(0,16,0,16),Text="",TextColor3=Color3.new(0,0,0),TextSize=14,ZIndex=10,}},
      {24,"TextLabel",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Font=3,Name="Me",Parent={19},Position=UDim2.new(0,5,0,20),Size=UDim2.new(1,-10,0,20),Text="Me Only",TextColor3=Color3.new(1,1,1),TextSize=14,TextXAlignment=0,ZIndex=10,}},
      {25,"Frame",{BackgroundColor3=Color3.new(0.30588236451149,0.30588236451149,0.3098039329052),BorderSizePixel=0,Name="Button",Parent={24},Position=UDim2.new(1,-20,0,0),Size=UDim2.new(0,20,0,20),ZIndex=10,}},
      {26,"TextButton",{BackgroundColor3=Color3.new(0.58823531866074,0.58823531866074,0.59215688705444),BackgroundTransparency=1,BorderSizePixel=0,Font=3,Name="On",Parent={25},Position=UDim2.new(0,2,0,2),Size=UDim2.new(0,16,0,16),Text="",TextColor3=Color3.new(0,0,0),TextSize=14,ZIndex=10,}},
      {27,"TextBox",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BorderColor3=Color3.new(0.15686275064945,0.15686275064945,0.15686275064945),BorderSizePixel=0,ClearTextOnFocus=false,Font=3,Name="Custom",Parent={19},PlaceholderColor3=Color3.new(0.47058826684952,0.47058826684952,0.47058826684952),PlaceholderText="Custom Player Set",Position=UDim2.new(0,5,0,64),Size=UDim2.new(1,-35,0,20),Text="",TextColor3=Color3.new(1,1,1),TextSize=14,TextXAlignment=0,ZIndex=10,}},
      {28,"Frame",{BackgroundColor3=Color3.new(0.30588236451149,0.30588236451149,0.3098039329052),BorderSizePixel=0,Name="CustomButton",Parent={19},Position=UDim2.new(1,-25,0,64),Size=UDim2.new(0,20,0,20),ZIndex=10,}},
      {29,"TextButton",{BackgroundColor3=Color3.new(0.58823531866074,0.58823531866074,0.59215688705444),BackgroundTransparency=1,BorderSizePixel=0,Font=3,Name="On",Parent={28},Position=UDim2.new(0,2,0,2),Size=UDim2.new(0,16,0,16),Text="",TextColor3=Color3.new(0,0,0),TextSize=14,ZIndex=10,}},
      {30,"Frame",{BackgroundColor3=Color3.new(0.19607844948769,0.19607844948769,0.19607844948769),BackgroundTransparency=1,BorderColor3=Color3.new(0.15686275064945,0.15686275064945,0.15686275064945),Name="Strings",Parent={18},Position=UDim2.new(0,0,0,25),Size=UDim2.new(1,0,0,64),Visible=false,ZIndex=10,}},
      {31,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Title",Parent={30},Size=UDim2.new(1,0,0,20),Text="Choose String",TextColor3=Color3.new(1,1,1),TextSize=14,ZIndex=10,}},
      {32,"TextLabel",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Font=3,Name="Any",Parent={30},Position=UDim2.new(0,5,0,20),Size=UDim2.new(1,-10,0,20),Text="Any String",TextColor3=Color3.new(1,1,1),TextSize=14,TextXAlignment=0,ZIndex=10,}},
      {33,"Frame",{BackgroundColor3=Color3.new(0.30588236451149,0.30588236451149,0.3098039329052),BorderSizePixel=0,Name="Button",Parent={32},Position=UDim2.new(1,-20,0,0),Size=UDim2.new(0,20,0,20),ZIndex=10,}},
      {34,"TextButton",{BackgroundColor3=Color3.new(0.58823531866074,0.58823531866074,0.59215688705444),BackgroundTransparency=1,BorderSizePixel=0,Font=3,Name="On",Parent={33},Position=UDim2.new(0,2,0,2),Size=UDim2.new(0,16,0,16),Text="",TextColor3=Color3.new(0,0,0),TextSize=14,ZIndex=10,}},
      {54,"Frame",{BackgroundColor3=Color3.new(0.19607844948769,0.19607844948769,0.19607844948769),BackgroundTransparency=1,BorderColor3=Color3.new(0.15686275064945,0.15686275064945,0.15686275064945),Name="Numbers",Parent={18},Position=UDim2.new(0,0,0,25),Size=UDim2.new(1,0,0,64),Visible=false,ZIndex=10,}},
      {55,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Title",Parent={54},Size=UDim2.new(1,0,0,20),Text="Choose String",TextColor3=Color3.new(1,1,1),TextSize=14,ZIndex=10,}},
      {56,"TextLabel",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Font=3,Name="Any",Parent={54},Position=UDim2.new(0,5,0,20),Size=UDim2.new(1,-10,0,20),Text="Any Number",TextColor3=Color3.new(1,1,1),TextSize=14,TextXAlignment=0,ZIndex=10,}},
      {57,"Frame",{BackgroundColor3=Color3.new(0.30588236451149,0.30588236451149,0.3098039329052),BorderSizePixel=0,Name="Button",Parent={56},Position=UDim2.new(1,-20,0,0),Size=UDim2.new(0,20,0,20),ZIndex=10,}},
      {58,"TextButton",{BackgroundColor3=Color3.new(0.58823531866074,0.58823531866074,0.59215688705444),BackgroundTransparency=1,BorderSizePixel=0,Font=3,Name="On",Parent={57},Position=UDim2.new(0,2,0,2),Size=UDim2.new(0,16,0,16),Text="",TextColor3=Color3.new(0,0,0),TextSize=14,ZIndex=10,}},
      {59,"TextBox",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BorderColor3=Color3.new(0.15686275064945,0.15686275064945,0.15686275064945),BorderSizePixel=0,ClearTextOnFocus=false,Font=3,Name="Custom",Parent={54},PlaceholderColor3=Color3.new(0.47058826684952,0.47058826684952,0.47058826684952),PlaceholderText="Number",Position=UDim2.new(0,5,0,42),Size=UDim2.new(1,-35,0,20),Text="",TextColor3=Color3.new(1,1,1),TextSize=14,TextXAlignment=0,ZIndex=10,}},
      {60,"Frame",{BackgroundColor3=Color3.new(0.30588236451149,0.30588236451149,0.3098039329052),BorderSizePixel=0,Name="CustomButton",Parent={54},Position=UDim2.new(1,-25,0,42),Size=UDim2.new(0,20,0,20),ZIndex=10,}},
      {61,"TextButton",{BackgroundColor3=Color3.new(0.58823531866074,0.58823531866074,0.59215688705444),BackgroundTransparency=1,BorderSizePixel=0,Font=3,Name="On",Parent={60},Position=UDim2.new(0,2,0,2),Size=UDim2.new(0,16,0,16),Text="",TextColor3=Color3.new(0,0,0),TextSize=14,ZIndex=10,}},
      {35,"TextBox",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BorderColor3=Color3.new(0.15686275064945,0.15686275064945,0.15686275064945),BorderSizePixel=0,ClearTextOnFocus=false,Font=3,Name="Custom",Parent={30},PlaceholderColor3=Color3.new(0.47058826684952,0.47058826684952,0.47058826684952),PlaceholderText="Match String",Position=UDim2.new(0,5,0,42),Size=UDim2.new(1,-35,0,20),Text="",TextColor3=Color3.new(1,1,1),TextSize=14,TextXAlignment=0,ZIndex=10,}},
      {36,"Frame",{BackgroundColor3=Color3.new(0.30588236451149,0.30588236451149,0.3098039329052),BorderSizePixel=0,Name="CustomButton",Parent={30},Position=UDim2.new(1,-25,0,42),Size=UDim2.new(0,20,0,20),ZIndex=10,}},
      {37,"TextButton",{BackgroundColor3=Color3.new(0.58823531866074,0.58823531866074,0.59215688705444),BackgroundTransparency=1,BorderSizePixel=0,Font=3,Name="On",Parent={36},Position=UDim2.new(0,2,0,2),Size=UDim2.new(0,16,0,16),Text="",TextColor3=Color3.new(0,0,0),TextSize=14,ZIndex=10,}},
      {38,"Frame",{BackgroundColor3=Color3.new(0.19607844948769,0.19607844948769,0.19607844948769),BackgroundTransparency=1,BorderColor3=Color3.new(0.15686275064945,0.15686275064945,0.15686275064945),Name="DelayEditor",Parent={18},Position=UDim2.new(0,0,0,25),Size=UDim2.new(1,0,0,24),Visible=false,ZIndex=10,}},
      {39,"TextBox",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BorderColor3=Color3.new(0.15686275064945,0.15686275064945,0.15686275064945),BorderSizePixel=0,Font=3,Name="Secs",Parent={38},PlaceholderColor3=Color3.new(0.47058826684952,0.47058826684952,0.47058826684952),Position=UDim2.new(0,60,0,2),Size=UDim2.new(1,-65,0,20),Text="",TextColor3=Color3.new(1,1,1),TextSize=14,TextXAlignment=0,ZIndex=10,}},
      {40,"TextLabel",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Font=3,Name="Label",Parent={39},Position=UDim2.new(0,-55,0,0),Size=UDim2.new(1,0,1,0),Text="Delay (s):",TextColor3=Color3.new(1,1,1),TextSize=14,TextXAlignment=0,ZIndex=10,}},
      {41,"Frame",{BackgroundColor3=currentShade1,BorderSizePixel=0,ClipsDescendants=true,Name="EventTemplate",Parent={6},Size=UDim2.new(1,0,0,20),Visible=false,ZIndex=10,}},
      {42,"TextButton",{BackgroundColor3=currentText1,BackgroundTransparency=1,Font=3,Name="Expand",Parent={41},Size=UDim2.new(0,20,0,20),Text=">",TextColor3=Color3.new(1,1,1),TextSize=18,ZIndex=10,}},
      {43,"TextLabel",{BackgroundColor3=currentText1,BackgroundTransparency=1,Font=3,Name="EventName",Parent={41},Position=UDim2.new(0,25,0,0),Size=UDim2.new(1,-25,0,20),Text="OnSpawn",TextColor3=Color3.new(1,1,1),TextSize=14,TextXAlignment=0,ZIndex=10,}},
      {44,"Frame",{BackgroundColor3=Color3.new(0.19607844948769,0.19607844948769,0.19607844948769),BorderSizePixel=0,BackgroundTransparency=1,ClipsDescendants=true,Name="Cmds",Parent={41},Position=UDim2.new(0,0,0,20),Size=UDim2.new(1,0,1,-20),ZIndex=10,}},
      {45,"Frame",{BackgroundColor3=Color3.new(0.14117647707462,0.14117647707462,0.14509804546833),BorderColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),Name="Add",Parent={44},Position=UDim2.new(0,0,1,-20),Size=UDim2.new(1,0,0,20),ZIndex=10,}},
      {46,"TextBox",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,ClearTextOnFocus=false,Font=3,Parent={45},PlaceholderColor3=Color3.new(0.7843137383461,0.7843137383461,0.7843137383461),PlaceholderText="Add new command",Position=UDim2.new(0,5,0,0),Size=UDim2.new(1,-10,1,0),Text="",TextColor3=Color3.new(1,1,1),TextSize=14,TextXAlignment=0,ZIndex=10,}},
      {47,"Frame",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Name="Holder",Parent={44},Size=UDim2.new(1,0,1,-20),ZIndex=10,}},
      {48,"UIListLayout",{Parent={47},SortOrder=2,}},
      {49,"Frame",{currentShade1,BorderSizePixel=0,ClipsDescendants=true,Name="CmdTemplate",Parent={6},Size=UDim2.new(1,0,0,20),Visible=false,ZIndex=10,}},
      {50,"TextBox",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,ClearTextOnFocus=false,Font=3,Parent={49},PlaceholderColor3=Color3.new(1,1,1),Position=UDim2.new(0,5,0,0),Size=UDim2.new(1,-45,0,20),Text="a\\b\\c\\d",TextColor3=currentText1,TextSize=14,TextXAlignment=0,ZIndex=10,}},
      {51,"TextButton",{BackgroundColor3=currentShade1,BorderSizePixel=0,Font=3,Name="Delete",Parent={49},Position=UDim2.new(1,-20,0,0),Size=UDim2.new(0,20,0,20),Text="X",TextColor3=Color3.new(1,1,1),TextSize=18,ZIndex=10,}},
      {52,"TextButton",{BackgroundColor3=currentShade1,BorderSizePixel=0,Font=3,Name="Settings",Parent={49},Position=UDim2.new(1,-40,0,0),Size=UDim2.new(0,20,0,20),Text="",TextColor3=Color3.new(1,1,1),TextSize=18,ZIndex=10,}},
      {53,"ImageLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Image="rbxassetid://1204397029",Parent={52},Position=UDim2.new(0,2,0,2),Size=UDim2.new(0,16,0,16),ZIndex=10,}},
    })
    main.Name = randomString()
    local mainFrame = main:WaitForChild("Content")
    local eventList = mainFrame:WaitForChild("List")
    local eventListHolder = eventList:WaitForChild("Holder")
    local cmdTemplate = mainFrame:WaitForChild("CmdTemplate")
    local eventTemplate = mainFrame:WaitForChild("EventTemplate")
    local settingsFrame = mainFrame:WaitForChild("Settings"):WaitForChild("Slider")
    local settingsTemplates = mainFrame.Settings:WaitForChild("Templates")
    local settingsList = settingsFrame:WaitForChild("List"):WaitForChild("Holder")
    table.insert(shade2,main.TopBar) table.insert(shade1,mainFrame) table.insert(shade2,eventTemplate)
    table.insert(text1,eventTemplate.EventName) table.insert(shade1,eventTemplate.Cmds.Add) table.insert(shade1,cmdTemplate)
    table.insert(text1,cmdTemplate.TextBox) table.insert(shade2,cmdTemplate.Delete) table.insert(shade2,cmdTemplate.Settings)
    table.insert(scroll,mainFrame.List) table.insert(shade1,settingsFrame) table.insert(shade2,settingsFrame.Line)
    table.insert(shade2,settingsFrame.Close) table.insert(scroll,settingsFrame.List) table.insert(shade2,settingsTemplates.DelayEditor.Secs)
    table.insert(text1,settingsTemplates.DelayEditor.Secs) table.insert(text1,settingsTemplates.DelayEditor.Secs.Label) table.insert(text1,settingsTemplates.Players.Title)
    table.insert(shade3,settingsTemplates.Players.CustomButton) table.insert(shade2,settingsTemplates.Players.Custom) table.insert(text1,settingsTemplates.Players.Custom)
    table.insert(shade3,settingsTemplates.Players.Any.Button) table.insert(shade3,settingsTemplates.Players.Me.Button) table.insert(text1,settingsTemplates.Players.Any)
    table.insert(text1,settingsTemplates.Players.Me) table.insert(text1,settingsTemplates.Strings.Title) table.insert(text1,settingsTemplates.Strings.Any)
    table.insert(shade3,settingsTemplates.Strings.Any.Button) table.insert(shade3,settingsTemplates.Strings.CustomButton) table.insert(text1,settingsTemplates.Strings.Custom)
    table.insert(shade2,settingsTemplates.Strings.Custom)
    table.insert(text1,settingsTemplates.Players.Me) table.insert(text1,settingsTemplates.Numbers.Title) table.insert(text1,settingsTemplates.Numbers.Any)
    table.insert(shade3,settingsTemplates.Numbers.Any.Button) table.insert(shade3,settingsTemplates.Numbers.CustomButton) table.insert(text1,settingsTemplates.Numbers.Custom)
    table.insert(shade2,settingsTemplates.Numbers.Custom)
  
    local tweenInf = TweenInfo.new(0.25,Enum.EasingStyle.Quart,Enum.EasingDirection.Out)
  
    local currentlyEditingCmd = nil
  
    settingsFrame:WaitForChild("Close").MouseButton1Click:Connect(function()
      settingsFrame:TweenPosition(UDim2.new(0,-150,0,0),Enum.EasingDirection.Out,Enum.EasingStyle.Quart,0.25,true)
    end)
  
    local function resizeList()
      local size = 0
  
      for i,v in pairs(eventListHolder:GetChildren()) do
        if v.Name == "EventTemplate" then
          size = size + 20
          if v.Expand.Rotation == 90 then
            size = size + 20*(1+(#events[v.EventName:GetAttribute("RawName")].commands or 0))
          end
        end
      end
  
      TweenService:Create(eventList,tweenInf,{CanvasSize = UDim2.new(0,0,0,size)}):Play()
  
      if size > eventList.AbsoluteSize.Y then
        eventListHolder.Size = UDim2.new(1,-8,1,0)
      else
        eventListHolder.Size = UDim2.new(1,0,1,0)
      end
    end
  
    local function resizeSettingsList()
      local size = 0
  
      for i,v in pairs(settingsList:GetChildren()) do
        if v:IsA("Frame") then
          size = size + v.AbsoluteSize.Y
        end
      end
  
      settingsList.Parent.CanvasSize = UDim2.new(0,0,0,size)
  
      if size > settingsList.Parent.AbsoluteSize.Y then
        settingsList.Size = UDim2.new(1,-8,1,0)
      else
        settingsList.Size = UDim2.new(1,0,1,0)
      end
    end
  
    local function setupCheckbox(button,callback)
      local enabled = button.On.BackgroundTransparency == 0
  
      local function update()
        button.On.BackgroundTransparency = (enabled and 0 or 1)
      end
  
      button.On.MouseButton1Click:Connect(function()
        enabled = not enabled
        update()
        if callback then callback(enabled) end
      end)
  
      return {
        Toggle = function(nocall) enabled = not enabled update() if not nocall and callback then callback(enabled) end end,
        Enable = function(nocall) if enabled then return end enabled = true update()if not nocall and callback then callback(enabled) end end,
        Disable = function(nocall) if not enabled then return end enabled = false update()if not nocall and callback then callback(enabled) end end,
        IsEnabled = function() return enabled end
      }
    end
  
    local function openSettingsEditor(event,cmd)
      currentlyEditingCmd = cmd
  
      for i,v in pairs(settingsList:GetChildren()) do if v:IsA("Frame") then v:Destroy() end end
  
      local delayEditor = settingsTemplates.DelayEditor:Clone()
      delayEditor.Secs.FocusLost:Connect(function()
        cmd[3] = tonumber(delayEditor.Secs.Text) or 0
        delayEditor.Secs.Text = cmd[3]
        if onEdited then onEdited() end
      end)
      delayEditor.Secs.Text = cmd[3]
      delayEditor.Visible = true
      table.insert(shade2,delayEditor.Secs)
      table.insert(text1,delayEditor.Secs)
      table.insert(text1,delayEditor.Secs.Label)
      delayEditor.Parent = settingsList
  
      for i,v in pairs(event.sets) do
        if v.Type == "Player" then
          local template = settingsTemplates.Players:Clone()
          template.Title.Text = v.Name or "Player"
  
          local me,any,custom
  
          me = setupCheckbox(template.Me.Button,function(on)
            if not on then return end
            any.Disable()
            custom.Disable()
            cmd[2][i] = 0
            if onEdited then onEdited() end
          end)
  
          any = setupCheckbox(template.Any.Button,function(on)
            if not on then return end
            me.Disable()
            custom.Disable()
            cmd[2][i] = 1
            if onEdited then onEdited() end
          end)
  
          local customTextBox = template.Custom
          custom = setupCheckbox(template.CustomButton,function(on)
            if not on then return end
            me.Disable()
            any.Disable()
            cmd[2][i] = customTextBox.Text
            if onEdited then onEdited() end
          end)
  
          ViewportTextBox.convert(customTextBox)
          customTextBox.FocusLost:Connect(function()
            if custom:IsEnabled() then
              cmd[2][i] = customTextBox.Text
              if onEdited then onEdited() end
            end
          end)
  
          local cVal = cmd[2][i]
          if cVal == 0 then
            me:Enable()
          elseif cVal == 1 then
            any:Enable()
          else
            custom:Enable()
            customTextBox.Text = cVal
          end
  
          template.Visible = true
          table.insert(text1,template.Title)
          table.insert(shade3,template.CustomButton)
          table.insert(shade3,template.Any.Button)
          table.insert(shade3,template.Me.Button)
          table.insert(text1,template.Any)
          table.insert(text1,template.Me)
          template.Parent = settingsList
        elseif v.Type == "String" then
          local template = settingsTemplates.Strings:Clone()
          template.Title.Text = v.Name or "String"
  
          local any,custom
  
          any = setupCheckbox(template.Any.Button,function(on)
            if not on then return end
            custom.Disable()
            cmd[2][i] = 0
            if onEdited then onEdited() end
          end)
  
          local customTextBox = template.Custom
          custom = setupCheckbox(template.CustomButton,function(on)
            if not on then return end
            any.Disable()
            cmd[2][i] = customTextBox.Text
            if onEdited then onEdited() end
          end)
  
          ViewportTextBox.convert(customTextBox)
          customTextBox.FocusLost:Connect(function()
            if custom:IsEnabled() then
              cmd[2][i] = customTextBox.Text
              if onEdited then onEdited() end
            end
          end)
  
          local cVal = cmd[2][i]
          if cVal == 0 then
            any:Enable()
          else
            custom:Enable()
            customTextBox.Text = cVal
          end
  
          template.Visible = true
          table.insert(text1,template.Title)
          table.insert(text1,template.Any)
          table.insert(shade3,template.Any.Button)
          table.insert(shade3,template.CustomButton)
          template.Parent = settingsList
        elseif v.Type == "Number" then
          local template = settingsTemplates.Numbers:Clone()
          template.Title.Text = v.Name or "Number"
  
          local any,custom
  
          any = setupCheckbox(template.Any.Button,function(on)
            if not on then return end
            custom.Disable()
            cmd[2][i] = 0
            if onEdited then onEdited() end
          end)
  
          local customTextBox = template.Custom
          custom = setupCheckbox(template.CustomButton,function(on)
            if not on then return end
            any.Disable()
            cmd[2][i] = customTextBox.Text
            if onEdited then onEdited() end
          end)
  
          ViewportTextBox.convert(customTextBox)
          customTextBox.FocusLost:Connect(function()
            cmd[2][i] = tonumber(customTextBox.Text) or 0
            customTextBox.Text = cmd[2][i]
            if custom:IsEnabled() then
              if onEdited then onEdited() end
            end
          end)
  
          local cVal = cmd[2][i]
          if cVal == 0 then
            any:Enable()
          else
            custom:Enable()
            customTextBox.Text = cVal
          end
  
          template.Visible = true
          table.insert(text1,template.Title)
          table.insert(text1,template.Any)
          table.insert(shade3,template.Any.Button)
          table.insert(shade3,template.CustomButton)
          template.Parent = settingsList
        end
      end
      resizeSettingsList()
      settingsFrame:TweenPosition(UDim2.new(0,0,0,0),Enum.EasingDirection.Out,Enum.EasingStyle.Quart,0.25,true)
    end
  
    local function defaultSettings(ev)
      local res = {}
  
      for i,v in pairs(ev.sets) do
        if v.Type == "Player" then
          res[#res+1] = v.Default or 0
        elseif v.Type == "String" then
          res[#res+1] = v.Default or 0
        elseif v.Type == "Number" then
          res[#res+1] = v.Default or 0
        end
      end
  
      return res
    end
  
    local function refreshList()
      for i,v in pairs(eventListHolder:GetChildren()) do if v:IsA("Frame") then v:Destroy() end end
  
      for name,event in pairs(events) do
        local eventF = eventTemplate:Clone()
        eventF.EventName.Text = name
        eventF.Visible = true
        eventF.EventName:SetAttribute("RawName", name)
        table.insert(shade2,eventF)
        table.insert(text1,eventF.EventName)
        table.insert(shade1,eventF.Cmds.Add)
  
        local expanded = false
        eventF.Expand.MouseButton1Down:Connect(function()
          expanded = not expanded
          eventF:TweenSize(UDim2.new(1,0,0,20 + (expanded and 20*#eventF.Cmds.Holder:GetChildren() or 0)),Enum.EasingDirection.Out,Enum.EasingStyle.Quart,0.25,true)
          eventF.Expand.Rotation = expanded and 90 or 0
          resizeList()
        end)
  
        local function refreshCommands()
          for i,v in pairs(eventF.Cmds.Holder:GetChildren()) do
            if v.Name == "CmdTemplate" then
              v:Destroy()
            end
          end
  
          eventF.EventName.Text = name..(#event.commands > 0 and " ("..#event.commands..")" or "")
  
          for i,cmd in pairs(event.commands) do
            local cmdF = cmdTemplate:Clone()
            local cmdTextBox = cmdF.TextBox
            ViewportTextBox.convert(cmdTextBox)
            cmdTextBox.Text = cmd[1]
            cmdF.Visible = true
            table.insert(shade1,cmdF)
            table.insert(shade2,cmdF.Delete)
            table.insert(shade2,cmdF.Settings)
  
            cmdTextBox.FocusLost:Connect(function()
              event.commands[i] = {cmdTextBox.Text,cmd[2],cmd[3]}
              if onEdited then onEdited() end
            end)
  
            cmdF.Settings.MouseButton1Click:Connect(function()
              openSettingsEditor(event,cmd)
            end)
  
            cmdF.Delete.MouseButton1Click:Connect(function()
              table.remove(event.commands,i)
              refreshCommands()
              resizeList()
  
              if currentlyEditingCmd == cmd then
                settingsFrame:TweenPosition(UDim2.new(0,-150,0,0),Enum.EasingDirection.Out,Enum.EasingStyle.Quart,0.25,true)
              end
              if onEdited then onEdited() end
            end)
  
            cmdF.Parent = eventF.Cmds.Holder
          end
  
          eventF:TweenSize(UDim2.new(1,0,0,20 + (expanded and 20*#eventF.Cmds.Holder:GetChildren() or 0)),Enum.EasingDirection.Out,Enum.EasingStyle.Quart,0.25,true)
        end
  
        local newBox = eventF.Cmds.Add.TextBox
        ViewportTextBox.convert(newBox)
        newBox.FocusLost:Connect(function(enter)
          if enter then
            event.commands[#event.commands+1] = {newBox.Text,defaultSettings(event),0}
            newBox.Text = ""
  
            refreshCommands()
            resizeList()
            if onEdited then onEdited() end
          end
        end)
  
        --eventF:GetPropertyChangedSignal("AbsoluteSize"):Connect(resizeList)
  
        eventF.Parent = eventListHolder
  
        refreshCommands()
      end
  
      resizeList()
    end
  
    local function saveData()
      local result = {}
      for i,v in pairs(events) do
        result[i] = v.commands
      end
      return HttpService:JSONEncode(result)
    end
  
    local function loadData(str)
      local data = HttpService:JSONDecode(str)
      for i,v in pairs(data) do
        if events[i] then
          events[i].commands = v
        end
      end
    end
  
    local function addCmd(event,data)
      table.insert(events[event].commands,data)
    end
  
    local function setOnEdited(f)
      if type(f) == "function" then
        onEdited = f
      end
    end
  
    main.TopBar.Close.MouseButton1Click:Connect(function()
      main:TweenPosition(UDim2.new(0.5,-175,0,-500), "InOut", "Quart", 0.5, true, nil)
    end)
    dragGUI(main)
    main.Parent = PARENT
  
    return {
      RegisterEvent = registerEvent,
      FireEvent = fireEvent,
      Refresh = refreshList,
      SaveData = saveData,
      LoadData = loadData,
      AddCmd = addCmd,
      Frame = main,
      SetOnEdited = setOnEdited
    }
  end)()
  
  reference = (function()
    local main = create({
      {1,"Frame",{BackgroundColor3=Color3.new(0.14117647707462,0.14117647707462,0.14509804546833),BackgroundTransparency=1,BorderColor3=Color3.new(0.15686275064945,0.15686275064945,0.15686275064945),BorderSizePixel=0,Name="Main",Position=UDim2.new(0.5,-250,0,-500),Size=UDim2.new(0,500,0,20),ZIndex=10,}},
      {2,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BorderSizePixel=0,Name="TopBar",Parent={1},Size=UDim2.new(1,0,0,20),ZIndex=10,}},
      {3,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Title",Parent={2},Size=UDim2.new(1,0,0.94999998807907,0),Text="Reference",TextColor3=Color3.new(1,1,1),TextSize=14,ZIndex=10,}},
      {4,"TextButton",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Close",Parent={2},Position=UDim2.new(1,-20,0,0),Size=UDim2.new(0,20,0,20),Text="",TextColor3=Color3.new(1,1,1),TextSize=14,ZIndex=10,}},
      {5,"ImageLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Image="rbxassetid://5054663650",Parent={4},Position=UDim2.new(0,5,0,5),Size=UDim2.new(0,10,0,10),ZIndex=10,}},
      {6,"Frame",{BackgroundColor3=Color3.new(0.14117647707462,0.14117647707462,0.14509804546833),BorderSizePixel=0,Name="Content",Parent={1},Position=UDim2.new(0,0,0,20),Size=UDim2.new(1,0,0,300),ZIndex=10,}},
      {7,"ScrollingFrame",{BackgroundColor3=Color3.new(0.14117647707462,0.14117647707462,0.14509804546833),BackgroundTransparency=1,BorderColor3=Color3.new(0.15686275064945,0.15686275064945,0.15686275064945),BorderSizePixel=0,BottomImage="rbxasset://textures/ui/Scroll/scroll-middle.png",CanvasSize=UDim2.new(0,0,0,1313),Name="List",Parent={6},ScrollBarImageColor3=Color3.new(0.30588236451149,0.30588236451149,0.3098039329052),ScrollBarThickness=8,Size=UDim2.new(1,0,1,0),TopImage="rbxasset://textures/ui/Scroll/scroll-middle.png",VerticalScrollBarInset=2,ZIndex=10,}},
      {8,"UIListLayout",{Parent={7},SortOrder=2,}},
      {9,"Frame",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Name="Section",Parent={7},Size=UDim2.new(1,0,0,429),ZIndex=10,}},
      {10,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="Header",Parent={9},Position=UDim2.new(0,8,0,5),Size=UDim2.new(1,-8,0,20),Text="Special Player Cases",TextColor3=Color3.new(1,1,1),TextSize=20,TextXAlignment=0,ZIndex=10,}},
      {11,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Text",Parent={9},Position=UDim2.new(0,8,0,25),Size=UDim2.new(1,-8,0,20),Text="These keywords can be used to quickly select groups of players in commands:",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {12,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BorderSizePixel=0,Name="Line",Parent={9},Position=UDim2.new(0,10,1,-1),Size=UDim2.new(1,-20,0,1),ZIndex=10,}},
      {13,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Cases",Parent={9},Position=UDim2.new(0,8,0,55),Size=UDim2.new(1,-16,0,342),ZIndex=10,}},
      {14,"UIListLayout",{Parent={13},SortOrder=2,}},
      {15,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,LayoutOrder=-4,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {16,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={15},Size=UDim2.new(1,0,1,0),Text="all",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {17,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={15},Position=UDim2.new(0,15,0,0),Size=UDim2.new(1,0,1,0),Text="- includes everyone",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {18,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,LayoutOrder=-3,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {19,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={18},Size=UDim2.new(1,0,1,0),Text="others",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {20,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={18},Position=UDim2.new(0,37,0,0),Size=UDim2.new(1,0,1,0),Text="- includes everyone except you",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {21,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,LayoutOrder=-2,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {22,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={21},Size=UDim2.new(1,0,1,0),Text="me",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {23,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={21},Position=UDim2.new(0,19,0,0),Size=UDim2.new(1,0,1,0),Text="- includes your player only",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {24,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {25,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={24},Size=UDim2.new(1,0,1,0),Text="#[number]",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {26,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={24},Position=UDim2.new(0,59,0,0),Size=UDim2.new(1,0,1,0),Text="- gets a specified amount of random players",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {27,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {28,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={27},Size=UDim2.new(1,0,1,0),Text="random",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {29,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={27},Position=UDim2.new(0,44,0,0),Size=UDim2.new(1,0,1,0),Text="- affects a random player",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {30,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {31,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={30},Size=UDim2.new(1,0,1,0),Text="%[team name]",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {32,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={30},Position=UDim2.new(0,78,0,0),Size=UDim2.new(1,0,1,0),Text="- includes everyone on a given team",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {33,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {34,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={33},Size=UDim2.new(1,0,1,0),Text="allies / team",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {35,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={33},Position=UDim2.new(0,63,0,0),Size=UDim2.new(1,0,1,0),Text="- players who are on your team",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {36,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {37,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={36},Size=UDim2.new(1,0,1,0),Text="enemies / nonteam",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {38,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={36},Position=UDim2.new(0,101,0,0),Size=UDim2.new(1,0,1,0),Text="- players who are not on your team",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {39,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {40,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={39},Size=UDim2.new(1,0,1,0),Text="friends",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {41,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={39},Position=UDim2.new(0,40,0,0),Size=UDim2.new(1,0,1,0),Text="- anyone who is friends with you",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {42,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {43,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={42},Size=UDim2.new(1,0,1,0),Text="nonfriends",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {44,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={42},Position=UDim2.new(0,61,0,0),Size=UDim2.new(1,0,1,0),Text="- anyone who is not friends with you",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {45,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {46,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={45},Size=UDim2.new(1,0,1,0),Text="guests",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {47,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={45},Position=UDim2.new(0,36,0,0),Size=UDim2.new(1,0,1,0),Text="- guest players (obsolete)",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {48,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {49,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={48},Size=UDim2.new(1,0,1,0),Text="bacons",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {50,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={48},Position=UDim2.new(0,40,0,0),Size=UDim2.new(1,0,1,0),Text="- anyone with the \"bacon\" or pal hair",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {51,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {52,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={51},Size=UDim2.new(1,0,1,0),Text="age[number]",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {53,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={51},Position=UDim2.new(0,71,0,0),Size=UDim2.new(1,0,1,0),Text="- includes anyone below or at the given age",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {54,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {55,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={54},Size=UDim2.new(1,0,1,0),Text="rad[number]",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {56,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={54},Position=UDim2.new(0,70,0,0),Size=UDim2.new(1,0,1,0),Text="- includes anyone within the given radius",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {57,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {58,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={57},Size=UDim2.new(1,0,1,0),Text="nearest",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {59,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={57},Position=UDim2.new(0,43,0,0),Size=UDim2.new(1,0,1,0),Text="- gets the closest player to you",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {60,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {61,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={60},Size=UDim2.new(1,0,1,0),Text="farthest",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {62,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={60},Position=UDim2.new(0,46,0,0),Size=UDim2.new(1,0,1,0),Text="- gets the farthest player from you",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {63,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {64,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={63},Size=UDim2.new(1,0,1,0),Text="group[ID]",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {65,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={63},Position=UDim2.new(0,55,0,0),Size=UDim2.new(1,0,1,0),Text="- gets players who are in a certain group",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {66,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {67,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={66},Size=UDim2.new(1,0,1,0),Text="alive",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {68,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={66},Position=UDim2.new(0,27,0,0),Size=UDim2.new(1,0,1,0),Text="- gets players who are alive",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {69,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {70,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={69},Size=UDim2.new(1,0,1,0),Text="dead",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {71,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={69},Position=UDim2.new(0,29,0,0),Size=UDim2.new(1,0,1,0),Text="- gets players who are dead",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {72,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BackgroundTransparency=1,BorderSizePixel=0,LayoutOrder=-1,Name="Case",Parent={13},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,0,0,18),ZIndex=10,}},
      {73,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="CaseName",Parent={72},Size=UDim2.new(1,0,1,0),Text="@username",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {74,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="CaseDesc",Parent={72},Position=UDim2.new(0,66,0,0),Size=UDim2.new(1,0,1,0),Text="- searches for players by username only (ignores displaynames)",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {75,"Frame",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Name="Section",Parent={7},Size=UDim2.new(1,0,0,180),ZIndex=10,}},
      {76,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="Header",Parent={75},Position=UDim2.new(0,8,0,5),Size=UDim2.new(1,-8,0,20),Text="Various Operators",TextColor3=Color3.new(1,1,1),TextSize=20,TextXAlignment=0,ZIndex=10,}},
      {77,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BorderSizePixel=0,Name="Line",Parent={75},Position=UDim2.new(0,10,1,-1),Size=UDim2.new(1,-20,0,1),ZIndex=10,}},
      {78,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="Text",Parent={75},Position=UDim2.new(0,8,0,30),Size=UDim2.new(1,-8,0,16),Text="Use commas to separate multiple expressions:",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,TextYAlignment=0,ZIndex=10,}},
      {79,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="Text",Parent={75},Position=UDim2.new(0,8,0,75),Size=UDim2.new(1,-8,0,16),Text="Use - to exclude, and + to include players in your expression:",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,TextYAlignment=0,ZIndex=10,}},
      {80,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Text",Parent={75},Position=UDim2.new(0,8,0,91),Size=UDim2.new(1,-8,0,16),Text=";locate %blue-friends (gets players in blue team who aren't your friends)",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,TextYAlignment=0,ZIndex=10,}},
      {81,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Text",Parent={75},Position=UDim2.new(0,8,0,46),Size=UDim2.new(1,-8,0,16),Text=";locate noob,noob2,bob",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,TextYAlignment=0,ZIndex=10,}},
      {82,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="Text",Parent={75},Position=UDim2.new(0,8,0,120),Size=UDim2.new(1,-8,0,16),Text="Put ! before a command to run it with the last arguments it was ran with:",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,TextYAlignment=0,ZIndex=10,}},
      {83,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Text",Parent={75},Position=UDim2.new(0,8,0,136),Size=UDim2.new(1,-8,0,32),Text="After running ;offset 0 100 0,  you can run !offset anytime to repeat that command with the same arguments that were used to run it last time",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,TextYAlignment=0,ZIndex=10,}},
      {84,"Frame",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Name="Section",Parent={7},Size=UDim2.new(1,0,0,154),ZIndex=10,}},
      {85,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="Header",Parent={84},Position=UDim2.new(0,8,0,5),Size=UDim2.new(1,-8,0,20),Text="Command Looping",TextColor3=Color3.new(1,1,1),TextSize=20,TextXAlignment=0,ZIndex=10,}},
      {86,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="Text",Parent={84},Position=UDim2.new(0,8,0,30),Size=UDim2.new(1,-8,0,20),Text="Form: [How many times it loops]^[delay (optional)]^[command]",TextColor3=Color3.new(1,1,1),TextSize=15,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {87,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BorderSizePixel=0,Name="Line",Parent={84},Position=UDim2.new(0,10,1,-1),Size=UDim2.new(1,-20,0,1),ZIndex=10,}},
      {88,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Text",Parent={84},Position=UDim2.new(0,8,0,50),Size=UDim2.new(1,-8,0,20),Text="Use the 'breakloops' command to stop all running loops.",TextColor3=Color3.new(1,1,1),TextSize=15,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {89,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="Text",Parent={84},Position=UDim2.new(0,8,0,80),Size=UDim2.new(1,-8,0,16),Text="Examples:",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,TextYAlignment=0,ZIndex=10,}},
      {90,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Text",Parent={84},Position=UDim2.new(0,8,0,98),Size=UDim2.new(1,-8,0,42),Text=";5^btools - gives you 5 sets of btools\n;10^3^drophats - drops your hats every 3 seconds 10 times\n;inf^0.1^animspeed 100 - infinitely loops your animation speed to 100",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,TextYAlignment=0,ZIndex=10,}},
      {91,"Frame",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Name="Section",Parent={7},Size=UDim2.new(1,0,0,120),ZIndex=10,}},
      {92,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="Header",Parent={91},Position=UDim2.new(0,8,0,5),Size=UDim2.new(1,-8,0,20),Text="Execute Multiple Commands at Once",TextColor3=Color3.new(1,1,1),TextSize=20,TextXAlignment=0,ZIndex=10,}},
      {93,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="Text",Parent={91},Position=UDim2.new(0,8,0,30),Size=UDim2.new(1,-8,0,20),Text="You can execute multiple commands at once using \"\\\"",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {94,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BorderSizePixel=0,Name="Line",Parent={91},Position=UDim2.new(0,10,1,-1),Size=UDim2.new(1,-20,0,1),ZIndex=10,}},
      {95,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="Text",Parent={91},Position=UDim2.new(0,8,0,60),Size=UDim2.new(1,-8,0,16),Text="Examples:",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,TextYAlignment=0,ZIndex=10,}},
      {96,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Text",Parent={91},Position=UDim2.new(0,8,0,78),Size=UDim2.new(1,-8,0,32),Text=";drophats\\respawn - drops your hats and respawns you\n;enable inventory\\enable playerlist\\refresh - enables those coregui items and refreshes you",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,TextYAlignment=0,ZIndex=10,}},
      {97,"Frame",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Name="Section",Parent={7},Size=UDim2.new(1,0,0,75),ZIndex=10,}},
      {98,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="Header",Parent={97},Position=UDim2.new(0,8,0,5),Size=UDim2.new(1,-8,0,20),Text="Browse Command History",TextColor3=Color3.new(1,1,1),TextSize=20,TextXAlignment=0,ZIndex=10,}},
      {99,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Text",Parent={97},Position=UDim2.new(0,8,0,30),Size=UDim2.new(1,-8,0,32),Text="While focused on the command bar, you can use the up and down arrow keys to browse recently used commands",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {100,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BorderSizePixel=0,Name="Line",Parent={97},Position=UDim2.new(0,10,1,-1),Size=UDim2.new(1,-20,0,1),ZIndex=10,}},
      {101,"Frame",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Name="Section",Parent={7},Size=UDim2.new(1,0,0,75),ZIndex=10,}},
      {102,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="Header",Parent={101},Position=UDim2.new(0,8,0,5),Size=UDim2.new(1,-8,0,20),Text="Autocomplete in the Command Bar",TextColor3=Color3.new(1,1,1),TextSize=20,TextXAlignment=0,ZIndex=10,}},
      {103,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Text",Parent={101},Position=UDim2.new(0,8,0,30),Size=UDim2.new(1,-8,0,32),Text="While focused on the command bar, you can use the tab key to insert the top suggested command into the command bar.",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {104,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BorderSizePixel=0,Name="Line",Parent={101},Position=UDim2.new(0,10,1,-1),Size=UDim2.new(1,-20,0,1),ZIndex=10,}},
      {105,"Frame",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Name="Section",Parent={7},Size=UDim2.new(1,0,0,175),ZIndex=10,}},
      {106,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="Header",Parent={105},Position=UDim2.new(0,8,0,5),Size=UDim2.new(1,-8,0,20),Text="Using Event Binds",TextColor3=Color3.new(1,1,1),TextSize=20,TextXAlignment=0,ZIndex=10,}},
      {107,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Text",Parent={105},Position=UDim2.new(0,8,0,30),Size=UDim2.new(1,-8,0,32),Text="Use event binds to set up commands that get executed when certain events happen. You can edit the conditions for an event command to run (such as which player triggers it).",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {108,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BorderSizePixel=0,Name="Line",Parent={105},Position=UDim2.new(0,10,1,-1),Size=UDim2.new(1,-20,0,1),ZIndex=10,}},
      {109,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Text",Parent={105},Position=UDim2.new(0,8,0,70),Size=UDim2.new(1,-8,0,48),Text="Some events may send arguments; you can use them in your event command by using $ followed by the argument number ($1, $2, etc). You can find out the order and types of these arguments by looking at the settings of the event command.",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {110,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="Text",Parent={105},Position=UDim2.new(0,8,0,130),Size=UDim2.new(1,-8,0,16),Text="Example:",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,TextYAlignment=0,ZIndex=10,}},
      {111,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Text",Parent={105},Position=UDim2.new(0,8,0,148),Size=UDim2.new(1,-8,0,16),Text="Setting up 'goto $1' on the OnChatted event will teleport you to any player that chats.",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,TextYAlignment=0,ZIndex=10,}},
      {112,"Frame",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Name="Section",Parent={7},Size=UDim2.new(1,0,0,105),ZIndex=10,}},
      {113,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=4,Name="Header",Parent={112},Position=UDim2.new(0,8,0,5),Size=UDim2.new(1,-8,0,20),Text="Get Further Help",TextColor3=Color3.new(1,1,1),TextSize=20,TextXAlignment=0,ZIndex=10,}},
      {114,"TextLabel",{BackgroundColor3=Color3.new(1,1,1),BackgroundTransparency=1,Font=3,Name="Text",Parent={112},Position=UDim2.new(0,8,0,30),Size=UDim2.new(1,-8,0,32),Text="You can join the Discord server to get support with IY,  and read up on more documentation such as the Plugin API.",TextColor3=Color3.new(1,1,1),TextSize=14,TextWrapped=true,TextXAlignment=0,ZIndex=10,}},
      {115,"Frame",{BackgroundColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),BorderSizePixel=0,Name="Line",Parent={112},Position=UDim2.new(0,10,1,-1),Size=UDim2.new(1,-20,0,1),Visible=false,ZIndex=10,}},
      {116,"TextButton",{BackgroundColor3=Color3.new(0.48627451062202,0.61960786581039,0.85098040103912),BorderColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),Font=4,Name="InviteButton",Parent={112},Position=UDim2.new(0,5,0,75),Size=UDim2.new(1,-10,0,25),Text="Copy Discord Invite Link (https://discord.gg/78ZuWSq)",TextColor3=Color3.new(0.1803921610117,0.1803921610117,0.1843137294054),TextSize=16,ZIndex=10,}},
    })
    for i,v in pairs(main.Content.List:GetDescendants()) do
      if v:IsA("TextLabel") then
        table.insert(text1,v)
      end
    end
    table.insert(scroll,main.Content.List)
    table.insert(shade1,main.Content)
    table.insert(shade2,main.TopBar)
    main.Name = randomString()
    main.TopBar.Close.MouseButton1Click:Connect(function()
      main:TweenPosition(UDim2.new(0.5,-250,0,-500), "InOut", "Quart", 0.5, true, nil)
    end)
    local inviteButton = main:FindFirstChild("InviteButton",true)
    local lastPress = nil
    inviteButton.MouseButton1Click:Connect(function()
      if everyClipboard then
        toClipboard("https://discord.gg/78ZuWSq")
        inviteButton.Text = "Copied"
      else
        inviteButton.Text = "No Clipboard Function, type out the link"
      end
      local pressTime = tick()
      lastPress = pressTime
      wait(2)
      if lastPress ~= pressTime then return end
      inviteButton.Text = "Copy Discord Invite Link (https://discord.gg/78ZuWSq)"
    end)
    dragGUI(main)
    main.Parent = PARENT
  
    ReferenceButton.MouseButton1Click:Connect(function()
      main:TweenPosition(UDim2.new(0.5,-250,0.5,-150), "InOut", "Quart", 0.5, true, nil)
    end)
  end)()
  
  currentShade1 = Color3.fromRGB(36, 36, 37)
  currentShade2 = Color3.fromRGB(46, 46, 47)
  currentShade3 = Color3.fromRGB(78, 78, 79)
  currentText1 = Color3.new(1, 1, 1)
  currentText2 = Color3.new(0, 0, 0)
  currentScroll = Color3.fromRGB(78,78,79)
  
  defaultsettings = {
    prefix = ';';
    StayOpen = false;
    espTransparency = 0.3;
    keepIY = true;
    logsEnabled = false;
    jLogsEnabled = false;
    aliases = {};
    binds = {};
    WayPoints = {};
    PluginsTable = {};
    currentShade1 = {currentShade1.R,currentShade1.G,currentShade1.B};
    currentShade2 = {currentShade2.R,currentShade2.G,currentShade2.B};
    currentShade3 = {currentShade3.R,currentShade3.G,currentShade3.B};
    currentText1 = {currentText1.R,currentText1.G,currentText1.B};
    currentText2 = {currentText2.R,currentText2.G,currentText2.B};
    currentScroll = {currentScroll.R,currentScroll.G,currentScroll.B};
    eventBinds = eventEditor.SaveData()
  }
  
  defaults = HttpService:JSONEncode(defaultsettings)
  nosaves = false
  useFactorySettings = function()
      prefix = ';'
      StayOpen = false
      KeepInfYield = true
      espTransparency = 0.3
      logsEnabled = false
      jLogsEnabled = false
      aliases = {}
      binds = {}
      WayPoints = {}
      PluginsTable = {}
  end
  
  local loadedEventData = nil
  local jsonAttempts = 0
  function saves()
    if writefileExploit() and jsonAttempts < 50 then
      if pcall(function() readfile("IY_FE.iy") end) then
        if readfile("IY_FE.iy") ~= nil then
          local success, response = pcall(function()
            local json = HttpService:JSONDecode(readfile("IY_FE.iy"))
            if json.prefix ~= nil then prefix = json.prefix else prefix = ';' end
            if json.StayOpen ~= nil then StayOpen = json.StayOpen else StayOpen = false end
            if json.keepIY ~= nil then KeepInfYield = json.keepIY else KeepInfYield = true end
            if json.espTransparency ~= nil then espTransparency = json.espTransparency else espTransparency = 0.3 end
            if json.logsEnabled ~= nil then logsEnabled = json.logsEnabled else logsEnabled = false end
            if json.jLogsEnabled ~= nil then jLogsEnabled = json.jLogsEnabled else jLogsEnabled = false end
            if json.aliases ~= nil then aliases = json.aliases else aliases = {} end
            if json.binds ~= nil then binds = (json.binds or {}) else binds = {} end
            if json.spawnCmds ~= nil then spawnCmds = json.spawnCmds end
            if json.WayPoints ~= nil then AllWaypoints = json.WayPoints else WayPoints = {} AllWaypoints = {} end
            if json.PluginsTable ~= nil then PluginsTable = json.PluginsTable else PluginsTable = {} end
            if json.currentShade1 ~= nil then currentShade1 = Color3.new(json.currentShade1[1],json.currentShade1[2],json.currentShade1[3]) end
            if json.currentShade2 ~= nil then currentShade2 = Color3.new(json.currentShade2[1],json.currentShade2[2],json.currentShade2[3]) end
            if json.currentShade3 ~= nil then currentShade3 = Color3.new(json.currentShade3[1],json.currentShade3[2],json.currentShade3[3]) end
            if json.currentText1 ~= nil then currentText1 = Color3.new(json.currentText1[1],json.currentText1[2],json.currentText1[3]) end
            if json.currentText2 ~= nil then currentText2 = Color3.new(json.currentText2[1],json.currentText2[2],json.currentText2[3]) end
            if json.currentScroll ~= nil then currentScroll = Color3.new(json.currentScroll[1],json.currentScroll[2],json.currentScroll[3]) end
            if json.eventBinds ~= nil then loadedEventData = json.eventBinds end
          end)
          if not success then
            jsonAttempts = jsonAttempts + 1
            warn("Save Json Error:", response)
            warn("Overwriting Save File")
            writefileCooldown("IY_FE.iy", defaults)
            wait()
            saves()
          end
        else
          writefileCooldown("IY_FE.iy", defaults)
          wait()
          saves()
        end
      else
        writefileCooldown("IY_FE.iy", defaults)
        wait()
        if pcall(function() readfile("IY_FE.iy") end) then
          saves()
        else
          nosaves = true
          useFactorySettings()
  
          local FileError = Instance.new("Frame")
          local background = Instance.new("Frame")
          local Directions = Instance.new("TextLabel")
          local shadow = Instance.new("Frame")
          local PopupText = Instance.new("TextLabel")
          local Exit = Instance.new("TextButton")
          local ExitImage = Instance.new("ImageLabel")
  
          FileError.Name = randomString()
          FileError.Parent = PARENT
          FileError.Active = true
          FileError.BackgroundTransparency = 1
          FileError.Position = UDim2.new(0.5, -180, 0, 290)
          FileError.Size = UDim2.new(0, 360, 0, 20)
          FileError.ZIndex = 10
  
          background.Name = "background"
          background.Parent = FileError
          background.Active = true
          background.BackgroundColor3 = Color3.fromRGB(36, 36, 37)
          background.BorderSizePixel = 0
          background.Position = UDim2.new(0, 0, 0, 20)
          background.Size = UDim2.new(0, 360, 0, 205)
          background.ZIndex = 10
  
          Directions.Name = "Directions"
          Directions.Parent = background
          Directions.BackgroundTransparency = 1
          Directions.BorderSizePixel = 0
          Directions.Position = UDim2.new(0, 10, 0, 10)
          Directions.Size = UDim2.new(0, 340, 0, 185)
          Directions.Font = Enum.Font.SourceSans
          Directions.TextSize = 14
          Directions.Text = "There was a problem writing a save file to your PC.\n\nPlease contact the developer/support team for your exploit and tell them writefile is not working.\n\nYour settings, keybinds, waypoints, and aliases will not save if you continue.\n\nThings to try:\n> Make sure a 'workspace' folder is located in the same folder as your exploit\n> If your exploit is inside of a zip/rar file, extract it.\n> Rejoin the game and try again or restart your PC and try again."
          Directions.TextColor3 = Color3.new(1, 1, 1)
          Directions.TextWrapped = true
          Directions.TextXAlignment = Enum.TextXAlignment.Left
          Directions.TextYAlignment = Enum.TextYAlignment.Top
          Directions.ZIndex = 10
  
          shadow.Name = "shadow"
          shadow.Parent = FileError
          shadow.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
          shadow.BorderSizePixel = 0
          shadow.Size = UDim2.new(0, 360, 0, 20)
          shadow.ZIndex = 10
  
          PopupText.Name = "PopupText"
          PopupText.Parent = shadow
          PopupText.BackgroundTransparency = 1
          PopupText.Size = UDim2.new(1, 0, 0.95, 0)
          PopupText.ZIndex = 10
          PopupText.Font = Enum.Font.SourceSans
          PopupText.TextSize = 14
          PopupText.Text = "File Error"
          PopupText.TextColor3 = Color3.new(1, 1, 1)
          PopupText.TextWrapped = true
  
          Exit.Name = "Exit"
          Exit.Parent = shadow
          Exit.BackgroundTransparency = 1
          Exit.Position = UDim2.new(1, -20, 0, 0)
          Exit.Size = UDim2.new(0, 20, 0, 20)
          Exit.Text = ""
          Exit.ZIndex = 10
  
          ExitImage.Parent = Exit
          ExitImage.BackgroundColor3 = Color3.new(1, 1, 1)
          ExitImage.BackgroundTransparency = 1
          ExitImage.Position = UDim2.new(0, 5, 0, 5)
          ExitImage.Size = UDim2.new(0, 10, 0, 10)
          ExitImage.Image = "rbxassetid://5054663650"
          ExitImage.ZIndex = 10
  
          Exit.MouseButton1Click:Connect(function()
            FileError:Destroy()
          end)
        end
      end
    else
          if jsonAttempts >= 50 then
              nosaves = true
              useFactorySettings()
  
              local FileError = Instance.new("Frame")
              local background = Instance.new("Frame")
              local Directions = Instance.new("TextLabel")
              local shadow = Instance.new("Frame")
              local PopupText = Instance.new("TextLabel")
              local Exit = Instance.new("TextButton")
              local ExitImage = Instance.new("ImageLabel")
  
              FileError.Name = randomString()
              FileError.Parent = PARENT
              FileError.Active = true
              FileError.BackgroundTransparency = 1
              FileError.Position = UDim2.new(0.5, -180, 0, 290)
              FileError.Size = UDim2.new(0, 360, 0, 20)
              FileError.ZIndex = 10
  
              background.Name = "background"
              background.Parent = FileError
              background.Active = true
              background.BackgroundColor3 = Color3.fromRGB(36, 36, 37)
              background.BorderSizePixel = 0
              background.Position = UDim2.new(0, 0, 0, 20)
              background.Size = UDim2.new(0, 360, 0, 205)
              background.ZIndex = 10
  
              Directions.Name = "Directions"
              Directions.Parent = background
              Directions.BackgroundTransparency = 1
              Directions.BorderSizePixel = 0
              Directions.Position = UDim2.new(0, 10, 0, 10)
              Directions.Size = UDim2.new(0, 340, 0, 185)
              Directions.Font = Enum.Font.SourceSans
              Directions.TextSize = 14
              Directions.Text = "Sorry, but we have attempted to parse your data, but it is unreadable!\n\nInfinite Yield is now using factory settings until your executor fixes its file system.\n\nYour data has not been deleted."
              Directions.TextColor3 = Color3.new(1, 1, 1)
              Directions.TextWrapped = true
              Directions.TextXAlignment = Enum.TextXAlignment.Left
              Directions.TextYAlignment = Enum.TextYAlignment.Top
              Directions.ZIndex = 10
  
              shadow.Name = "shadow"
              shadow.Parent = FileError
              shadow.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
              shadow.BorderSizePixel = 0
              shadow.Size = UDim2.new(0, 360, 0, 20)
              shadow.ZIndex = 10
  
              PopupText.Name = "PopupText"
              PopupText.Parent = shadow
              PopupText.BackgroundTransparency = 1
              PopupText.Size = UDim2.new(1, 0, 0.95, 0)
              PopupText.ZIndex = 10
              PopupText.Font = Enum.Font.SourceSans
              PopupText.TextSize = 14
              PopupText.Text = "File Error"
              PopupText.TextColor3 = Color3.new(1, 1, 1)
              PopupText.TextWrapped = true
  
              Exit.Name = "Exit"
              Exit.Parent = shadow
              Exit.BackgroundTransparency = 1
              Exit.Position = UDim2.new(1, -20, 0, 0)
              Exit.Size = UDim2.new(0, 20, 0, 20)
              Exit.Text = ""
              Exit.ZIndex = 10
  
              ExitImage.Parent = Exit
              ExitImage.BackgroundColor3 = Color3.new(1, 1, 1)
              ExitImage.BackgroundTransparency = 1
              ExitImage.Position = UDim2.new(0, 5, 0, 5)
              ExitImage.Size = UDim2.new(0, 10, 0, 10)
              ExitImage.Image = "rbxassetid://5054663650"
              ExitImage.ZIndex = 10
  
              Exit.MouseButton1Click:Connect(function()
                  FileError:Destroy()
              end)
          else
              useFactorySettings()
          end
    end
  end
  
  saves()
  
  function updatesaves()
    if nosaves == false and writefileExploit() then
      local update = {
        prefix = prefix;
        StayOpen = StayOpen;
        keepIY = KeepInfYield;
        espTransparency = espTransparency;
        logsEnabled = logsEnabled;
        jLogsEnabled = jLogsEnabled;
        aliases = aliases;
        binds = binds or {};
        WayPoints = AllWaypoints;
        PluginsTable = PluginsTable;
        currentShade1 = {currentShade1.R,currentShade1.G,currentShade1.B};
        currentShade2 = {currentShade2.R,currentShade2.G,currentShade2.B};
        currentShade3 = {currentShade3.R,currentShade3.G,currentShade3.B};
        currentText1 = {currentText1.R,currentText1.G,currentText1.B};
        currentText2 = {currentText2.R,currentText2.G,currentText2.B};
        currentScroll = {currentScroll.R,currentScroll.G,currentScroll.B};
        eventBinds = eventEditor.SaveData()
      }
      writefileCooldown("IY_FE.iy", HttpService:JSONEncode(update))
    end
  end
  
  eventEditor.SetOnEdited(updatesaves)
  
  pWayPoints = {}
  WayPoints = {}
  
  if #AllWaypoints > 0 then
    for i = 1, #AllWaypoints do
      if not AllWaypoints[i].GAME or AllWaypoints[i].GAME == PlaceId then
        WayPoints[#WayPoints + 1] = {NAME = AllWaypoints[i].NAME, COORD = {AllWaypoints[i].COORD[1], AllWaypoints[i].COORD[2], AllWaypoints[i].COORD[3]}, GAME = AllWaypoints[i].GAME}
      end
    end
  end
  
  if type(binds) ~= "table" then binds = {} end
  
  function Time()
    local HOUR = math.floor((tick() % 86400) / 3600)
    local MINUTE = math.floor((tick() % 3600) / 60)
    local SECOND = math.floor(tick() % 60)
    local AP = HOUR > 11 and 'PM' or 'AM'
    HOUR = (HOUR % 12 == 0 and 12 or HOUR % 12)
    HOUR = HOUR < 10 and '0' .. HOUR or HOUR
    MINUTE = MINUTE < 10 and '0' .. MINUTE or MINUTE
    SECOND = SECOND < 10 and '0' .. SECOND or SECOND
    return HOUR .. ':' .. MINUTE .. ':' .. SECOND .. ' ' .. AP
  end
  
  PrefixBox.Text = prefix
  local SettingsOpen = false
  local isHidden = false
  
  if StayOpen == false then
    On.BackgroundTransparency = 1
  else
    On.BackgroundTransparency = 0
  end
  
  if logsEnabled then
    Toggle.Text = 'Enabled'
  else
    Toggle.Text = 'Disabled'
  end
  
  if jLogsEnabled then
    Toggle_2.Text = 'Enabled'
  else
    Toggle_2.Text = 'Disabled'
  end
  
  function maximizeHolder()
    if StayOpen == false then
      Holder:TweenPosition(UDim2.new(1, Holder.Position.X.Offset, 1, -220), "InOut", "Quart", 0.2, true, nil)
    end
  end
  
  local minimizeNum = -20
  function minimizeHolder()
    if StayOpen == false then
      Holder:TweenPosition(UDim2.new(1, Holder.Position.X.Offset, 1, minimizeNum), "InOut", "Quart", 0.5, true, nil)
    end
  end
  
  function cmdbarHolder()
    if StayOpen == false then
      Holder:TweenPosition(UDim2.new(1, Holder.Position.X.Offset, 1, -45), "InOut", "Quart", 0.5, true, nil)
    end
  end
  
  pinNotification = nil
  local notifyCount = 0
  function notify(text,text2,length)
    task.spawn(function()
      local LnotifyCount = notifyCount+1
      local notificationPinned = false
      notifyCount = notifyCount+1
      if pinNotification then pinNotification:Disconnect() end
      pinNotification = PinButton.MouseButton1Click:Connect(function()
        task.spawn(function()
          pinNotification:Disconnect()
          notificationPinned = true
          Title_2.BackgroundTransparency = 1
          wait(0.5)
          Title_2.BackgroundTransparency = 0
        end)
      end)
      Notification:TweenPosition(UDim2.new(1, Notification.Position.X.Offset, 1, 0), "InOut", "Quart", 0.5, true, nil)
      wait(0.6)
      local closepressed = false
      if text2 then
        Title_2.Text = text
        Text_2.Text = text2
      else
        Title_2.Text = 'Notification'
        Text_2.Text = text
      end
      Notification:TweenPosition(UDim2.new(1, Notification.Position.X.Offset, 1, -100), "InOut", "Quart", 0.5, true, nil)
      CloseButton.MouseButton1Click:Connect(function()
        Notification:TweenPosition(UDim2.new(1, Notification.Position.X.Offset, 1, 0), "InOut", "Quart", 0.5, true, nil)
        closepressed = true
        pinNotification:Disconnect()
      end)
      if length and isNumber(length) then
        wait(length)
      else
        wait(10)
      end
      if LnotifyCount == notifyCount then
        if closepressed == false and notificationPinned == false then
          pinNotification:Disconnect()
          Notification:TweenPosition(UDim2.new(1, Notification.Position.X.Offset, 1, 0), "InOut", "Quart", 0.5, true, nil)
        end
        notifyCount = 0
      end
    end)
  end
  
  local lastMessage = nil
  local lastLabel = nil
  local dupeCount = 1
  function CreateLabel(Name, Text)
    if lastMessage == Name..Text then
      dupeCount = dupeCount+1
      lastLabel.Text = Time()..' - ['..Name..']: '..Text..' (x'..dupeCount..')'
    else
      if dupeCount > 1 then dupeCount = 1 end
      if #scroll_2:GetChildren() >= 2546 then
        scroll_2:ClearAllChildren()
      end
      local alls = 0
      for i,v in pairs(scroll_2:GetChildren()) do
        if v then
          alls = v.Size.Y.Offset + alls
        end
        if not v then
          alls = 0
        end
      end
      local tl = Instance.new('TextLabel')
      lastMessage = Name..Text
      lastLabel = tl
      tl.Name = Name
      tl.Parent = scroll_2
      tl.ZIndex = 10
      tl.Text = Time().." - ["..Name.."]: "..Text
      tl.Size = UDim2.new(0,322,0,84)
      tl.BackgroundTransparency = 1
      tl.BorderSizePixel = 0
      tl.Font = "SourceSans"
      tl.Position = UDim2.new(-1,0,0,alls)
      tl.TextTransparency = 1
      tl.TextScaled = false
      tl.TextSize = 14
      tl.TextWrapped = true
      tl.TextXAlignment = "Left"
      tl.TextYAlignment = "Top"
      tl.TextColor3 = currentText1
      tl.Size = UDim2.new(0,322,0,tl.TextBounds.Y)
      table.insert(text1,tl)
      scroll_2.CanvasSize = UDim2.new(0,0,0,alls+tl.TextBounds.Y)
      scroll_2.CanvasPosition = Vector2.new(0,scroll_2.CanvasPosition.Y+tl.TextBounds.Y)
      tl:TweenPosition(UDim2.new(0,3,0,alls), 'In', 'Quint', 0.5)
      TweenService:Create(tl, TweenInfo.new(1.25, Enum.EasingStyle.Linear), { TextTransparency = 0 }):Play()
    end
  end
  
  function CreateJoinLabel(plr,ID)
    if #scroll_3:GetChildren() >= 2546 then
      scroll_3:ClearAllChildren()
    end
    local infoFrame = Instance.new("Frame")
    local info1 = Instance.new("TextLabel")
    local info2 = Instance.new("TextLabel")
    local ImageLabel_3 = Instance.new("ImageLabel")
    infoFrame.Name = randomString()
    infoFrame.Parent = scroll_3
    infoFrame.BackgroundColor3 = Color3.new(1, 1, 1)
    infoFrame.BackgroundTransparency = 1
    infoFrame.BorderColor3 = Color3.new(0.105882, 0.164706, 0.207843)
    infoFrame.Size = UDim2.new(1, 0, 0, 50)
    info1.Name = randomString()
    info1.Parent = infoFrame
    info1.BackgroundTransparency = 1
    info1.BorderSizePixel = 0
    info1.Position = UDim2.new(0, 45, 0, 0)
    info1.Size = UDim2.new(0, 135, 1, 0)
    info1.ZIndex = 10
    info1.Font = Enum.Font.SourceSans
    info1.FontSize = Enum.FontSize.Size14
    info1.Text = "Username: "..plr.Name.."\nJoined Server: "..Time()
    info1.TextColor3 = Color3.new(1, 1, 1)
    info1.TextWrapped = true
    info1.TextXAlignment = Enum.TextXAlignment.Left
    info2.Name = randomString()
    info2.Parent = infoFrame
    info2.BackgroundTransparency = 1
    info2.BorderSizePixel = 0
    info2.Position = UDim2.new(0, 185, 0, 0)
    info2.Size = UDim2.new(0, 140, 1, -5)
    info2.ZIndex = 10
    info2.Font = Enum.Font.SourceSans
    info2.FontSize = Enum.FontSize.Size14
    info2.Text = "User ID: "..ID.."\nAccount Age: "..plr.AccountAge.."\nJoined Roblox: Loading..."
    info2.TextColor3 = Color3.new(1, 1, 1)
    info2.TextWrapped = true
    info2.TextXAlignment = Enum.TextXAlignment.Left
    info2.TextYAlignment = Enum.TextYAlignment.Center
    ImageLabel_3.Parent = infoFrame
    ImageLabel_3.BackgroundTransparency = 1
    ImageLabel_3.BorderSizePixel = 0
    ImageLabel_3.Size = UDim2.new(0, 45, 1, 0)
    ImageLabel_3.Image = Players:GetUserThumbnailAsync(ID, Enum.ThumbnailType.AvatarThumbnail, Enum.ThumbnailSize.Size420x420)
    scroll_3.CanvasSize = UDim2.new(0, 0, 0, listlayout.AbsoluteContentSize.Y)
    scroll_3.CanvasPosition = Vector2.new(0,scroll_2.CanvasPosition.Y+infoFrame.AbsoluteSize.Y)
    wait()
    local user = game:HttpGet("https://users.roblox.com/v1/users/"..ID)
    local json = HttpService:JSONDecode(user)
    local date = json["created"]:sub(1,10)
    local splitDates = string.split(date,"-")
    info2.Text = string.gsub(info2.Text, "Loading...",splitDates[2].."/"..splitDates[3].."/"..splitDates[1])
  end
  
  IYMouse.KeyDown:Connect(function(Key)
    if (Key==prefix) then
      Cmdbar:CaptureFocus()
      spawn(function()
        repeat Cmdbar.Text = '' until Cmdbar.Text == ''
      end)
      maximizeHolder()
    end
  end)
  
  local lastMinimizeReq = 0
  Holder.MouseEnter:Connect(function()
    lastMinimizeReq = 0
    maximizeHolder()
  end)
  
  Holder.MouseLeave:Connect(function()
    if not Cmdbar:IsFocused() then
      local reqTime = tick()
      lastMinimizeReq = reqTime
      wait(1)
      if lastMinimizeReq ~= reqTime then return end
      if not Cmdbar:IsFocused() then
        minimizeHolder()
      end
    end
  end)
  
  function updateColors(color,ctype)
    if ctype == shade1 then
      for i,v in pairs(shade1) do
        v.BackgroundColor3 = color
      end
      currentShade1 = color
    elseif ctype == shade2 then
      for i,v in pairs(shade2) do
        v.BackgroundColor3 = color
      end
      currentShade2 = color
    elseif ctype == shade3 then
      for i,v in pairs(shade3) do
        v.BackgroundColor3 = color
      end
      currentShade3 = color
    elseif ctype == text1 then
      for i,v in pairs(text1) do
        v.TextColor3 = color
        if v:IsA("TextBox") then
          v.PlaceholderColor3 = color	
        end
      end
      currentText1 = color
    elseif ctype == text2 then
      for i,v in pairs(text2) do
        v.TextColor3 = color
      end
      currentText2 = color
    elseif ctype == scroll then
      for i,v in pairs(scroll) do
        v.ScrollBarImageColor3 = color
      end
      currentScroll = color
    end
  end
  
  local colorpickerOpen = false
  ColorsButton.MouseButton1Click:Connect(function()
    cache_currentShade1 = currentShade1
    cache_currentShade2 = currentShade2
    cache_currentShade3 = currentShade3
    cache_currentText1 = currentText1
    cache_currentText2 = currentText2
    cache_currentScroll = currentScroll
    if not colorpickerOpen then
      colorpickerOpen = true
      picker = game:GetObjects("rbxassetid://4908465318")[1]
      picker.Name = randomString()
      picker.Parent = PARENT
  
      local ColorPicker do
        ColorPicker = {}
  
        ColorPicker.new = function()
          local newMt = setmetatable({},{})
  
          local pickerGui = picker.ColorPicker
          local pickerTopBar = pickerGui.TopBar
          local pickerExit = pickerTopBar.Exit
          local pickerFrame = pickerGui.Content
          local colorSpace = pickerFrame.ColorSpaceFrame.ColorSpace
          local colorStrip = pickerFrame.ColorStrip
          local previewFrame = pickerFrame.Preview
          local basicColorsFrame = pickerFrame.BasicColors
          local customColorsFrame = pickerFrame.CustomColors
          local defaultButton = pickerFrame.Default
          local cancelButton = pickerFrame.Cancel
          local shade1Button = pickerFrame.Shade1
          local shade2Button = pickerFrame.Shade2
          local shade3Button = pickerFrame.Shade3
          local text1Button = pickerFrame.Text1
          local text2Button = pickerFrame.Text2
          local scrollButton = pickerFrame.Scroll
  
          local colorScope = colorSpace.Scope
          local colorArrow = pickerFrame.ArrowFrame.Arrow
  
          local hueInput = pickerFrame.Hue.Input
          local satInput = pickerFrame.Sat.Input
          local valInput = pickerFrame.Val.Input
  
          local redInput = pickerFrame.Red.Input
          local greenInput = pickerFrame.Green.Input
          local blueInput = pickerFrame.Blue.Input
  
          local mouse = IYMouse
  
          local hue,sat,val = 0,0,1
          local red,green,blue = 1,1,1
          local chosenColor = Color3.new(0,0,0)
  
          local basicColors = {Color3.new(0,0,0),Color3.new(0.66666668653488,0,0),Color3.new(0,0.33333334326744,0),Color3.new(0.66666668653488,0.33333334326744,0),Color3.new(0,0.66666668653488,0),Color3.new(0.66666668653488,0.66666668653488,0),Color3.new(0,1,0),Color3.new(0.66666668653488,1,0),Color3.new(0,0,0.49803924560547),Color3.new(0.66666668653488,0,0.49803924560547),Color3.new(0,0.33333334326744,0.49803924560547),Color3.new(0.66666668653488,0.33333334326744,0.49803924560547),Color3.new(0,0.66666668653488,0.49803924560547),Color3.new(0.66666668653488,0.66666668653488,0.49803924560547),Color3.new(0,1,0.49803924560547),Color3.new(0.66666668653488,1,0.49803924560547),Color3.new(0,0,1),Color3.new(0.66666668653488,0,1),Color3.new(0,0.33333334326744,1),Color3.new(0.66666668653488,0.33333334326744,1),Color3.new(0,0.66666668653488,1),Color3.new(0.66666668653488,0.66666668653488,1),Color3.new(0,1,1),Color3.new(0.66666668653488,1,1),Color3.new(0.33333334326744,0,0),Color3.new(1,0,0),Color3.new(0.33333334326744,0.33333334326744,0),Color3.new(1,0.33333334326744,0),Color3.new(0.33333334326744,0.66666668653488,0),Color3.new(1,0.66666668653488,0),Color3.new(0.33333334326744,1,0),Color3.new(1,1,0),Color3.new(0.33333334326744,0,0.49803924560547),Color3.new(1,0,0.49803924560547),Color3.new(0.33333334326744,0.33333334326744,0.49803924560547),Color3.new(1,0.33333334326744,0.49803924560547),Color3.new(0.33333334326744,0.66666668653488,0.49803924560547),Color3.new(1,0.66666668653488,0.49803924560547),Color3.new(0.33333334326744,1,0.49803924560547),Color3.new(1,1,0.49803924560547),Color3.new(0.33333334326744,0,1),Color3.new(1,0,1),Color3.new(0.33333334326744,0.33333334326744,1),Color3.new(1,0.33333334326744,1),Color3.new(0.33333334326744,0.66666668653488,1),Color3.new(1,0.66666668653488,1),Color3.new(0.33333334326744,1,1),Color3.new(1,1,1)}
          local customColors = {}
  
          dragGUI(picker)
  
          local function updateColor(noupdate)
            local relativeX,relativeY,relativeStripY = 219 - hue*219, 199 - sat*199, 199 - val*199
            local hsvColor = Color3.fromHSV(hue,sat,val)
  
            if noupdate == 2 or not noupdate then
              hueInput.Text = tostring(math.ceil(359*hue))
              satInput.Text = tostring(math.ceil(255*sat))
              valInput.Text = tostring(math.floor(255*val))
            end
            if noupdate == 1 or not noupdate then
              redInput.Text = tostring(math.floor(255*red))
              greenInput.Text = tostring(math.floor(255*green))
              blueInput.Text = tostring(math.floor(255*blue))
            end
  
            chosenColor = Color3.new(red,green,blue)
  
            colorScope.Position = UDim2.new(0,relativeX-9,0,relativeY-9)
            colorStrip.ImageColor3 = Color3.fromHSV(hue,sat,1)
            colorArrow.Position = UDim2.new(0,-2,0,relativeStripY-4)
            previewFrame.BackgroundColor3 = chosenColor
  
            newMt.Color = chosenColor
            if newMt.Changed then newMt:Changed(chosenColor) end
          end
  
          local function colorSpaceInput()
            local relativeX = mouse.X - colorSpace.AbsolutePosition.X
            local relativeY = mouse.Y - colorSpace.AbsolutePosition.Y
  
            if relativeX < 0 then relativeX = 0 elseif relativeX > 219 then relativeX = 219 end
            if relativeY < 0 then relativeY = 0 elseif relativeY > 199 then relativeY = 199 end
  
            hue = (219 - relativeX)/219
            sat = (199 - relativeY)/199
  
            local hsvColor = Color3.fromHSV(hue,sat,val)
            red,green,blue = hsvColor.r,hsvColor.g,hsvColor.b
  
            updateColor()
          end
  
          local function colorStripInput()
            local relativeY = mouse.Y - colorStrip.AbsolutePosition.Y
  
            if relativeY < 0 then relativeY = 0 elseif relativeY > 199 then relativeY = 199 end	
  
            val = (199 - relativeY)/199
  
            local hsvColor = Color3.fromHSV(hue,sat,val)
            red,green,blue = hsvColor.r,hsvColor.g,hsvColor.b
  
            updateColor()
          end
  
          local function hookButtons(frame,func)
            frame.ArrowFrame.Up.InputBegan:Connect(function(input)
              if input.UserInputType == Enum.UserInputType.MouseMovement then
                frame.ArrowFrame.Up.BackgroundTransparency = 0.5
              elseif input.UserInputType == Enum.UserInputType.MouseButton1 then
                local releaseEvent,runEvent
  
                local startTime = tick()
                local pressing = true
                local startNum = tonumber(frame.Text)
  
                if not startNum then return end
  
                releaseEvent = UserInputService.InputEnded:Connect(function(input)
                  if input.UserInputType ~= Enum.UserInputType.MouseButton1 then return end
                  releaseEvent:Disconnect()
                  pressing = false
                end)
  
                startNum = startNum + 1
                func(startNum)
                while pressing do
                  if tick()-startTime > 0.3 then
                    startNum = startNum + 1
                    func(startNum)
                  end
                  wait(0.1)
                end
              end
            end)
  
            frame.ArrowFrame.Up.InputEnded:Connect(function(input)
              if input.UserInputType == Enum.UserInputType.MouseMovement then
                frame.ArrowFrame.Up.BackgroundTransparency = 1
              end
            end)
  
            frame.ArrowFrame.Down.InputBegan:Connect(function(input)
              if input.UserInputType == Enum.UserInputType.MouseMovement then
                frame.ArrowFrame.Down.BackgroundTransparency = 0.5
              elseif input.UserInputType == Enum.UserInputType.MouseButton1 then
                local releaseEvent,runEvent
  
                local startTime = tick()
                local pressing = true
                local startNum = tonumber(frame.Text)
  
                if not startNum then return end
  
                releaseEvent = UserInputService.InputEnded:Connect(function(input)
                  if input.UserInputType ~= Enum.UserInputType.MouseButton1 then return end
                  releaseEvent:Disconnect()
                  pressing = false
                end)
  
                startNum = startNum - 1
                func(startNum)
                while pressing do
                  if tick()-startTime > 0.3 then
                    startNum = startNum - 1
                    func(startNum)
                  end
                  wait(0.1)
                end
              end
            end)
  
            frame.ArrowFrame.Down.InputEnded:Connect(function(input)
              if input.UserInputType == Enum.UserInputType.MouseMovement then
                frame.ArrowFrame.Down.BackgroundTransparency = 1
              end
            end)
          end
  
          colorSpace.InputBegan:Connect(function(input)
            if input.UserInputType == Enum.UserInputType.MouseButton1 then
              local releaseEvent,mouseEvent
  
              releaseEvent = UserInputService.InputEnded:Connect(function(input)
                if input.UserInputType ~= Enum.UserInputType.MouseButton1 then return end
                releaseEvent:Disconnect()
                mouseEvent:Disconnect()
              end)
  
              mouseEvent = UserInputService.InputChanged:Connect(function(input)
                if input.UserInputType == Enum.UserInputType.MouseMovement then
                  colorSpaceInput()
                end
              end)
  
              colorSpaceInput()
            end
          end)
  
          colorStrip.InputBegan:Connect(function(input)
            if input.UserInputType == Enum.UserInputType.MouseButton1 then
              local releaseEvent,mouseEvent
  
              releaseEvent = UserInputService.InputEnded:Connect(function(input)
                if input.UserInputType ~= Enum.UserInputType.MouseButton1 then return end
                releaseEvent:Disconnect()
                mouseEvent:Disconnect()
              end)
  
              mouseEvent = UserInputService.InputChanged:Connect(function(input)
                if input.UserInputType == Enum.UserInputType.MouseMovement then
                  colorStripInput()
                end
              end)
  
              colorStripInput()
            end
          end)
  
          local function updateHue(str)
            local num = tonumber(str)
            if num then
              hue = math.clamp(math.floor(num),0,359)/359
              local hsvColor = Color3.fromHSV(hue,sat,val)
              red,green,blue = hsvColor.r,hsvColor.g,hsvColor.b
              hueInput.Text = tostring(hue*359)
              updateColor(1)
            end
          end
          hueInput.FocusLost:Connect(function() updateHue(hueInput.Text) end) hookButtons(hueInput,updateHue)
  
          local function updateSat(str)
            local num = tonumber(str)
            if num then
              sat = math.clamp(math.floor(num),0,255)/255
              local hsvColor = Color3.fromHSV(hue,sat,val)
              red,green,blue = hsvColor.r,hsvColor.g,hsvColor.b
              satInput.Text = tostring(sat*255)
              updateColor(1)
            end
          end
          satInput.FocusLost:Connect(function() updateSat(satInput.Text) end) hookButtons(satInput,updateSat)
  
          local function updateVal(str)
            local num = tonumber(str)
            if num then
              val = math.clamp(math.floor(num),0,255)/255
              local hsvColor = Color3.fromHSV(hue,sat,val)
              red,green,blue = hsvColor.r,hsvColor.g,hsvColor.b
              valInput.Text = tostring(val*255)
              updateColor(1)
            end
          end
          valInput.FocusLost:Connect(function() updateVal(valInput.Text) end) hookButtons(valInput,updateVal)
  
          local function updateRed(str)
            local num = tonumber(str)
            if num then
              red = math.clamp(math.floor(num),0,255)/255
              local newColor = Color3.new(red,green,blue)
              hue,sat,val = Color3.toHSV(newColor)
              redInput.Text = tostring(red*255)
              updateColor(2)
            end
          end
          redInput.FocusLost:Connect(function() updateRed(redInput.Text) end) hookButtons(redInput,updateRed)
  
          local function updateGreen(str)
            local num = tonumber(str)
            if num then
              green = math.clamp(math.floor(num),0,255)/255
              local newColor = Color3.new(red,green,blue)
              hue,sat,val = Color3.toHSV(newColor)
              greenInput.Text = tostring(green*255)
              updateColor(2)
            end
          end
          greenInput.FocusLost:Connect(function() updateGreen(greenInput.Text) end) hookButtons(greenInput,updateGreen)
  
          local function updateBlue(str)
            local num = tonumber(str)
            if num then
              blue = math.clamp(math.floor(num),0,255)/255
              local newColor = Color3.new(red,green,blue)
              hue,sat,val = Color3.toHSV(newColor)
              blueInput.Text = tostring(blue*255)
              updateColor(2)
            end
          end
          blueInput.FocusLost:Connect(function() updateBlue(blueInput.Text) end) hookButtons(blueInput,updateBlue)
  
          local colorChoice = Instance.new("TextButton")
          colorChoice.Name = "Choice"
          colorChoice.Size = UDim2.new(0,25,0,18)
          colorChoice.BorderColor3 = Color3.new(96/255,96/255,96/255)
          colorChoice.Text = ""
          colorChoice.AutoButtonColor = false
          colorChoice.ZIndex = 10
  
          local row = 0
          local column = 0
          for i,v in pairs(basicColors) do
            local newColor = colorChoice:Clone()
            newColor.BackgroundColor3 = v
            newColor.Position = UDim2.new(0,1 + 30*column,0,21 + 23*row)
  
            newColor.MouseButton1Click:Connect(function()
              red,green,blue = v.r,v.g,v.b
              local newColor = Color3.new(red,green,blue)
              hue,sat,val = Color3.toHSV(newColor)
              updateColor()
            end)	
  
            newColor.Parent = basicColorsFrame
            column = column + 1
            if column == 6 then row = row + 1 column = 0 end
          end
  
          row = 0
          column = 0
          for i = 1,12 do
            local color = customColors[i] or Color3.new(0,0,0)
            local newColor = colorChoice:Clone()
            newColor.BackgroundColor3 = color
            newColor.Position = UDim2.new(0,1 + 30*column,0,20 + 23*row)
  
            newColor.MouseButton1Click:Connect(function()
              local curColor = customColors[i] or Color3.new(0,0,0)
              red,green,blue = curColor.r,curColor.g,curColor.b
              hue,sat,val = Color3.toHSV(curColor)
              updateColor()
            end)
  
            newColor.MouseButton2Click:Connect(function()
              customColors[i] = chosenColor
              newColor.BackgroundColor3 = chosenColor
            end)
  
            newColor.Parent = customColorsFrame
            column = column + 1
            if column == 6 then row = row + 1 column = 0 end
          end
  
          shade1Button.MouseButton1Click:Connect(function() if newMt.Confirm then newMt:Confirm(chosenColor,shade1) end end)
          shade1Button.InputBegan:Connect(function(input) if input.UserInputType == Enum.UserInputType.MouseMovement then shade1Button.BackgroundTransparency = 0.4 end end)
          shade1Button.InputEnded:Connect(function(input) if input.UserInputType == Enum.UserInputType.MouseMovement then shade1Button.BackgroundTransparency = 0 end end)
  
          shade2Button.MouseButton1Click:Connect(function() if newMt.Confirm then newMt:Confirm(chosenColor,shade2) end end)
          shade2Button.InputBegan:Connect(function(input) if input.UserInputType == Enum.UserInputType.MouseMovement then shade2Button.BackgroundTransparency = 0.4 end end)
          shade2Button.InputEnded:Connect(function(input) if input.UserInputType == Enum.UserInputType.MouseMovement then shade2Button.BackgroundTransparency = 0 end end)
  
          shade3Button.MouseButton1Click:Connect(function() if newMt.Confirm then newMt:Confirm(chosenColor,shade3) end end)
          shade3Button.InputBegan:Connect(function(input) if input.UserInputType == Enum.UserInputType.MouseMovement then shade3Button.BackgroundTransparency = 0.4 end end)
          shade3Button.InputEnded:Connect(function(input) if input.UserInputType == Enum.UserInputType.MouseMovement then shade3Button.BackgroundTransparency = 0 end end)
  
          text1Button.MouseButton1Click:Connect(function() if newMt.Confirm then newMt:Confirm(chosenColor,text1) end end)
          text1Button.InputBegan:Connect(function(input) if input.UserInputType == Enum.UserInputType.MouseMovement then text1Button.BackgroundTransparency = 0.4 end end)
          text1Button.InputEnded:Connect(function(input) if input.UserInputType == Enum.UserInputType.MouseMovement then text1Button.BackgroundTransparency = 0 end end)
  
          text2Button.MouseButton1Click:Connect(function() if newMt.Confirm then newMt:Confirm(chosenColor,text2) end end)
          text2Button.InputBegan:Connect(function(input) if input.UserInputType == Enum.UserInputType.MouseMovement then text2Button.BackgroundTransparency = 0.4 end end)
          text2Button.InputEnded:Connect(function(input) if input.UserInputType == Enum.UserInputType.MouseMovement then text2Button.BackgroundTransparency = 0 end end)
  
          scrollButton.MouseButton1Click:Connect(function() if newMt.Confirm then newMt:Confirm(chosenColor,scroll) end end)
          scrollButton.InputBegan:Connect(function(input) if input.UserInputType == Enum.UserInputType.MouseMovement then scrollButton.BackgroundTransparency = 0.4 end end)
          scrollButton.InputEnded:Connect(function(input) if input.UserInputType == Enum.UserInputType.MouseMovement then scrollButton.BackgroundTransparency = 0 end end)
  
          cancelButton.MouseButton1Click:Connect(function() if newMt.Cancel then newMt:Cancel() end end)
          cancelButton.InputBegan:Connect(function(input) if input.UserInputType == Enum.UserInputType.MouseMovement then cancelButton.BackgroundTransparency = 0.4 end end)
          cancelButton.InputEnded:Connect(function(input) if input.UserInputType == Enum.UserInputType.MouseMovement then cancelButton.BackgroundTransparency = 0 end end)
  
          defaultButton.MouseButton1Click:Connect(function() if newMt.Default then newMt:Default() end end)
          defaultButton.InputBegan:Connect(function(input) if input.UserInputType == Enum.UserInputType.MouseMovement then defaultButton.BackgroundTransparency = 0.4 end end)
          defaultButton.InputEnded:Connect(function(input) if input.UserInputType == Enum.UserInputType.MouseMovement then defaultButton.BackgroundTransparency = 0 end end)
  
          pickerExit.MouseButton1Click:Connect(function()
            picker:TweenPosition(UDim2.new(0.5, -219, 0, -500), "InOut", "Quart", 0.5, true, nil)
          end)
  
          updateColor()
  
          newMt.SetColor = function(self,color)
            red,green,blue = color.r,color.g,color.b
            hue,sat,val = Color3.toHSV(color)
            updateColor()
          end
  
          return newMt
        end
      end
  
      picker:TweenPosition(UDim2.new(0.5, -219, 0, 100), "InOut", "Quart", 0.5, true, nil)
  
      local Npicker = ColorPicker.new()
      Npicker.Confirm = function(self,color,ctype) updateColors(color,ctype) wait() updatesaves() end
      Npicker.Cancel = function(self)
        updateColors(cache_currentShade1,shade1)
        updateColors(cache_currentShade2,shade2)
        updateColors(cache_currentShade3,shade3)
        updateColors(cache_currentText1,text1)
        updateColors(cache_currentText2,text2)
        updateColors(cache_currentScroll,scroll)
        wait()
        updatesaves()
      end
      Npicker.Default = function(self)
        updateColors(Color3.fromRGB(36, 36, 37),shade1)
        updateColors(Color3.fromRGB(46, 46, 47),shade2)
        updateColors(Color3.fromRGB(78, 78, 79),shade3)
        updateColors(Color3.new(1, 1, 1),text1)
        updateColors(Color3.new(0, 0, 0),text2)
        updateColors(Color3.fromRGB(78,78,79),scroll)
        wait()
        updatesaves()
      end
    else
      picker:TweenPosition(UDim2.new(0.5, -219, 0, 100), "InOut", "Quart", 0.5, true, nil)
    end
  end)
  
  
  SettingsButton.MouseButton1Click:Connect(function()
    if SettingsOpen == false then SettingsOpen = true
      Settings:TweenPosition(UDim2.new(0, 0, 0, 45), "InOut", "Quart", 0.5, true, nil)
      CMDsF.Visible = false
    else SettingsOpen = false
      CMDsF.Visible = true
      Settings:TweenPosition(UDim2.new(0, 0, 0, 220), "InOut", "Quart", 0.5, true, nil)
    end
  end)
  
  On.MouseButton1Click:Connect(function()
    if isHidden == false then
      if StayOpen == false then
        StayOpen = true
        On.BackgroundTransparency = 0
      else
        StayOpen = false
        On.BackgroundTransparency = 1
      end
      updatesaves()
    end
  end)
  
  Clear.MouseButton1Down:Connect(function()
    for _, child in pairs(scroll_2:GetChildren()) do
      child:Destroy()
    end
    scroll_2.CanvasSize = UDim2.new(0, 0, 0, 10)
  end)
  
  Clear_2.MouseButton1Down:Connect(function()
    for _, child in pairs(scroll_3:GetChildren()) do
      child:Destroy()
    end
    scroll_3.CanvasSize = UDim2.new(0, 0, 0, 10)
  end)
  
  Toggle.MouseButton1Down:Connect(function()
    if logsEnabled then
      logsEnabled = false
      Toggle.Text = 'Disabled'
      updatesaves()
    else
      logsEnabled = true
      Toggle.Text = 'Enabled'
      updatesaves()
    end
  end)
  
  Toggle_2.MouseButton1Down:Connect(function()
    if jLogsEnabled then
      jLogsEnabled = false
      Toggle_2.Text = 'Disabled'
      updatesaves()
    else
      jLogsEnabled = true
      Toggle_2.Text = 'Enabled'
      updatesaves()
    end
  end)
  
  selectChat.MouseButton1Down:Connect(function()
    join.Visible = false
    chat.Visible = true
    table.remove(shade3,table.find(shade3,selectChat))
    table.remove(shade2,table.find(shade2,selectJoin))
    table.insert(shade2,selectChat)
    table.insert(shade3,selectJoin)
    selectJoin.BackgroundColor3 = currentShade3
    selectChat.BackgroundColor3 = currentShade2
  end)
  
  selectJoin.MouseButton1Down:Connect(function()
    chat.Visible = false
    join.Visible = true	
    table.remove(shade3,table.find(shade3,selectJoin))
    table.remove(shade2,table.find(shade2,selectChat))
    table.insert(shade2,selectJoin)
    table.insert(shade3,selectChat)
    selectChat.BackgroundColor3 = currentShade3
    selectJoin.BackgroundColor3 = currentShade2
  end)
  
  if not writefileExploit() then
      notify("Saves", "Your exploit does not support read/write file. Your settings will not save.")
  end
  
  ChatLog = function(plr)
    plr.Chatted:Connect(function(Message)
      if logsEnabled == true then
        CreateLabel(plr.Name,Message)
      end
    end)
  end
  
  JoinLog = function(plr)
    if jLogsEnabled == true then
      CreateJoinLabel(plr,plr.UserId)
    end
  end
  
  CleanFileName = function(name)
      return tostring(name):gsub("[*\\?:<>|]+", ""):sub(1, 175)
  end
  
  SaveChatlogs.MouseButton1Down:Connect(function()
    if writefileExploit() then
      if #scroll_2:GetChildren() > 0 then
        notify("Loading",'Hold on a sec')
        local placeName = CleanFileName(MarketplaceService:GetProductInfo(PlaceId).Name)
        local writelogs = '-- Infinite Yield Chat logs for "'..placeName..'"\n'
        for _, child in pairs(scroll_2:GetChildren()) do
          writelogs = writelogs..'\n'..child.Text
        end
        local writelogsFile = tostring(writelogs)
        local fileext = 0
        local function nameFile()
          local file
          pcall(function() file = readfile(placeName..' Chat Logs ('..fileext..').txt') end)
          if file then
            fileext = fileext+1
            nameFile()
          else
            writefileCooldown(placeName..' Chat Logs ('..fileext..').txt', writelogsFile)
          end
        end
        nameFile()
        notify('Chat Logs','Saved chat logs to the workspace folder within your exploit folder.')
      end
    else
      notify('Chat Logs','Your exploit does not support write file. You cannot save chat logs.')
    end
  end)
  
  for _, plr in pairs(Players:GetPlayers()) do
      ChatLog(plr)
  end
  
  Players.PlayerRemoving:Connect(function(player)
    if ESPenabled or CHMSenabled or COREGUI:FindFirstChild(player.Name..'_LC') then
      for i,v in pairs(COREGUI:GetChildren()) do
        if v.Name == player.Name..'_ESP' or v.Name == player.Name..'_LC' or v.Name == player.Name..'_CHMS' then
          v:Destroy()
        end
      end
    end
    if viewing ~= nil and player == viewing then
      workspace.CurrentCamera.CameraSubject = Players.LocalPlayer.Character
      viewing = nil
      if viewDied then
        viewDied:Disconnect()
        viewChanged:Disconnect()
      end
      notify('Spectate','View turned off (player left)')
    end
    eventEditor.FireEvent("OnLeave", player.Name)
  end)
  
  Exit.MouseButton1Down:Connect(function()
    logs:TweenPosition(UDim2.new(0, 0, 1, 10), "InOut", "Quart", 0.3, true, nil)
  end)
  
  Hide.MouseButton1Down:Connect(function()
    if logs.Position ~= UDim2.new(0, 0, 1, -20) then
      logs:TweenPosition(UDim2.new(0, 0, 1, -20), "InOut", "Quart", 0.3, true, nil)
    else
      logs:TweenPosition(UDim2.new(0, 0, 1, -265), "InOut", "Quart", 0.3, true, nil)
    end
  end)
  
  EventBind.MouseButton1Click:Connect(function()
    eventEditor.Frame:TweenPosition(UDim2.new(0.5,-175,0.5,-101), "InOut", "Quart", 0.5, true, nil)
  end)
  
  Keybinds.MouseButton1Click:Connect(function()
    KeybindsFrame:TweenPosition(UDim2.new(0, 0, 0, 0), "InOut", "Quart", 0.5, true, nil)
    wait(0.5)
    SettingsHolder.Visible = false
  end)
  
  Close.MouseButton1Click:Connect(function()
    SettingsHolder.Visible = true
    KeybindsFrame:TweenPosition(UDim2.new(0, 0, 0, 175), "InOut", "Quart", 0.5, true, nil)
  end)
  
  Keybinds.MouseButton1Click:Connect(function()
    KeybindsFrame:TweenPosition(UDim2.new(0, 0, 0, 0), "InOut", "Quart", 0.5, true, nil)
    wait(0.5)
    SettingsHolder.Visible = false
  end)
  
  Add.MouseButton1Click:Connect(function()
    KeybindEditor:TweenPosition(UDim2.new(0.5, -180, 0, 260), "InOut", "Quart", 0.5, true, nil)
  end)
  
  Delete.MouseButton1Click:Connect(function()
    binds = {}
    refreshbinds()
    updatesaves()
    notify('Keybinds Updated','Removed all keybinds')
  end)
  
  Close_2.MouseButton1Click:Connect(function()
    SettingsHolder.Visible = true
    AliasesFrame:TweenPosition(UDim2.new(0, 0, 0, 175), "InOut", "Quart", 0.5, true, nil)
  end)
  
  Aliases.MouseButton1Click:Connect(function()
    AliasesFrame:TweenPosition(UDim2.new(0, 0, 0, 0), "InOut", "Quart", 0.5, true, nil)
    wait(0.5)
    SettingsHolder.Visible = false
  end)
  
  Close_3.MouseButton1Click:Connect(function()
    SettingsHolder.Visible = true
    PositionsFrame:TweenPosition(UDim2.new(0, 0, 0, 175), "InOut", "Quart", 0.5, true, nil)
  end)
  
  Positions.MouseButton1Click:Connect(function()
    PositionsFrame:TweenPosition(UDim2.new(0, 0, 0, 0), "InOut", "Quart", 0.5, true, nil)
    wait(0.5)
    SettingsHolder.Visible = false
  end)
  
  local selectionBox = Instance.new("SelectionBox")
  selectionBox.Name = randomString()
  selectionBox.Color3 = Color3.new(255,255,255)
  selectionBox.Adornee = nil
  selectionBox.Parent = PARENT
  
  local selected = Instance.new("SelectionBox")
  selected.Name = randomString()
  selected.Color3 = Color3.new(0,166,0)
  selected.Adornee = nil
  selected.Parent = PARENT
  
  local ActivateHighlight = nil
  local ClickSelect = nil
  function selectPart()
    ToPartFrame:TweenPosition(UDim2.new(0.5, -180, 0, 335), "InOut", "Quart", 0.5, true, nil)
    local function HighlightPart()
      if selected.Adornee ~= IYMouse.Target then
        selectionBox.Adornee = IYMouse.Target
      else
        selectionBox.Adornee = nil
      end
    end
    ActivateHighlight = IYMouse.Move:Connect(HighlightPart)
    local function SelectPart()
      if IYMouse.Target ~= nil then
        selected.Adornee = IYMouse.Target
        Path.Text = getHierarchy(IYMouse.Target)
      end
    end
    ClickSelect = IYMouse.Button1Down:Connect(SelectPart)
  end
  
  Part.MouseButton1Click:Connect(function()
    selectPart()
  end)
  
  Exit_4.MouseButton1Click:Connect(function()
    ToPartFrame:TweenPosition(UDim2.new(0.5, -180, 0, -500), "InOut", "Quart", 0.5, true, nil)
    if ActivateHighlight then
      ActivateHighlight:Disconnect()
    end
    if ClickSelect then
      ClickSelect:Disconnect()
    end
    selectionBox.Adornee = nil
    selected.Adornee = nil
    Path.Text = ""
  end)
  
  CopyPath.MouseButton1Click:Connect(function()
    if Path.Text ~= "" then
      toClipboard(Path.Text)
    else
      notify('Copy Path','Select a part to copy its path')
    end
  end)
  
  ChoosePart.MouseButton1Click:Connect(function()
    if Path.Text ~= "" then
      local tpNameExt = ''
      local function handleWpNames()
        local FoundDupe = false
        for i,v in pairs(pWayPoints) do
          if v.NAME:lower() == selected.Adornee.Name:lower()..tpNameExt then
            FoundDupe = true
          end
        end
        if not FoundDupe then
          notify('Modified Waypoints',"Created waypoint: "..selected.Adornee.Name..tpNameExt)
          pWayPoints[#pWayPoints + 1] = {NAME = selected.Adornee.Name..tpNameExt, COORD = {selected.Adornee}}
        else
          if isNumber(tpNameExt) then
            tpNameExt = tpNameExt+1
          else
            tpNameExt = 1
          end
          handleWpNames()
        end
      end
      handleWpNames()
      refreshwaypoints()
    else
      notify('Part Selection','Select a part first')
    end
  end)
  
  cmds={}
  customAlias = {}
  Delete_3.MouseButton1Click:Connect(function()
    customAlias = {}
    aliases = {}
    notify('Aliases Modified','Removed all aliases')
    updatesaves()
    refreshaliases()
  end)
  
  PrefixBox:GetPropertyChangedSignal("Text"):Connect(function()
    prefix = PrefixBox.Text
    Cmdbar.PlaceholderText = "Command Bar ("..prefix..")"
    updatesaves()
  end)
  
  function CamViewport()
    if workspace.CurrentCamera then
      return workspace.CurrentCamera.ViewportSize.X
    end
  end
  
  function UpdateToViewport()
    if Holder.Position.X.Offset < -CamViewport() then
      Holder:TweenPosition(UDim2.new(1, -CamViewport(), Holder.Position.Y.Scale, Holder.Position.Y.Offset), "InOut", "Quart", 0.04, true, nil)
      Notification:TweenPosition(UDim2.new(1, -CamViewport() + 250, Notification.Position.Y.Scale, Notification.Position.Y.Offset), "InOut", "Quart", 0.04, true, nil)
    end
  end
  CameraChanged = workspace.CurrentCamera:GetPropertyChangedSignal("ViewportSize"):Connect(UpdateToViewport)
  
  function updateCamera(child, parent)
    if parent ~= workspace then
      CamMoved:Disconnect()
      CameraChanged:Disconnect()
      repeat wait() until workspace.CurrentCamera
      CameraChanged = workspace.CurrentCamera:GetPropertyChangedSignal("ViewportSize"):Connect(UpdateToViewport)
      CamMoved = workspace.CurrentCamera.AncestryChanged:Connect(updateCamera)
    end
  end
  CamMoved = workspace.CurrentCamera.AncestryChanged:Connect(updateCamera)
  
  function dragMain(dragpoint,gui)
    task.spawn(function()
      local dragging
      local dragInput
      local dragStart = Vector3.new(0,0,0)
      local startPos
      local function update(input)
        local pos = -250
        local delta = input.Position - dragStart
        if startPos.X.Offset + delta.X <= -500 then
          local Position = UDim2.new(1, -250, Notification.Position.Y.Scale, Notification.Position.Y.Offset)
          TweenService:Create(Notification, TweenInfo.new(.20), {Position = Position}):Play()
          pos = 250
        else
          local Position = UDim2.new(1, -500, Notification.Position.Y.Scale, Notification.Position.Y.Offset)
          TweenService:Create(Notification, TweenInfo.new(.20), {Position = Position}):Play()
          pos = -250
        end
        if startPos.X.Offset + delta.X <= -250 and -CamViewport() <= startPos.X.Offset + delta.X then
          local Position = UDim2.new(startPos.X.Scale, startPos.X.Offset + delta.X, gui.Position.Y.Scale, gui.Position.Y.Offset)
          TweenService:Create(gui, TweenInfo.new(.20), {Position = Position}):Play()
          local Position2 = UDim2.new(startPos.X.Scale, startPos.X.Offset + delta.X + pos, Notification.Position.Y.Scale, Notification.Position.Y.Offset)
          TweenService:Create(Notification, TweenInfo.new(.20), {Position = Position2}):Play()
        elseif startPos.X.Offset + delta.X > -500 then
          local Position = UDim2.new(1, -250, gui.Position.Y.Scale, gui.Position.Y.Offset)
          TweenService:Create(gui, TweenInfo.new(.20), {Position = Position}):Play()
        elseif -CamViewport() > startPos.X.Offset + delta.X then
          gui:TweenPosition(UDim2.new(1, -CamViewport(), gui.Position.Y.Scale, gui.Position.Y.Offset), "InOut", "Quart", 0.04, true, nil)
          local Position = UDim2.new(1, -CamViewport(), gui.Position.Y.Scale, gui.Position.Y.Offset)
          TweenService:Create(gui, TweenInfo.new(.20), {Position = Position}):Play()
          local Position2 = UDim2.new(1, -CamViewport() + 250, Notification.Position.Y.Scale, Notification.Position.Y.Offset)
          TweenService:Create(Notification, TweenInfo.new(.20), {Position = Position2}):Play()
        end
      end
      dragpoint.InputBegan:Connect(function(input)
        if input.UserInputType == Enum.UserInputType.MouseButton1 or input.UserInputType == Enum.UserInputType.Touch then
          dragging = true
          dragStart = input.Position
          startPos = gui.Position
  
          input.Changed:Connect(function()
            if input.UserInputState == Enum.UserInputState.End then
              dragging = false
            end
          end)
        end
      end)
      dragpoint.InputChanged:Connect(function(input)
        if input.UserInputType == Enum.UserInputType.MouseMovement or input.UserInputType == Enum.UserInputType.Touch then
          dragInput = input
        end
      end)
      UserInputService.InputChanged:Connect(function(input)
        if input == dragInput and dragging then
          update(input)
        end
      end)
    end)
  end
  
  dragMain(Title,Holder)
  
  Match = function(name,str)
    str = str:gsub("%W", "%%%1")
    return name:lower():find(str:lower()) and true
  end
  
  local canvasPos = Vector2.new(0,0)
  local topCommand = nil
  IndexContents = function(str,bool,cmdbar,Ianim)
    CMDsF.CanvasPosition = Vector2.new(0,0)
    local SizeY = 0
    local indexnum = 0
    local frame = CMDsF
    topCommand = nil
    local chunks = {}
    if str:sub(#str,#str) == "\\" then str = "" end
    for w in string.gmatch(str,"[^\\]+") do
      table.insert(chunks,w)
    end
    if #chunks > 0 then str = chunks[#chunks] end
    if str:sub(1,1) == "!" then str = str:sub(2) end
    for i,v in next, frame:GetChildren() do
      if v:IsA("TextButton") then
        if bool then
          if Match(v.Text,str) then
            indexnum = indexnum + 1
            v.Visible = true
            if topCommand == nil then
              topCommand = v.Text
            end
          else
            v.Visible = false
          end
        else
          v.Visible = true
          if topCommand == nil then
            topCommand = v.Text
          end
        end
      end
    end
    frame.CanvasSize = UDim2.new(0,0,0,cmdListLayout.AbsoluteContentSize.Y)
    if not Ianim then
      if indexnum == 0 or string.find(str, " ") then
        if not cmdbar then
          minimizeHolder()
        elseif cmdbar then
          cmdbarHolder()
        end
      else
        maximizeHolder()
      end
    else
      minimizeHolder()
    end
  end
  
  task.spawn(function()
    if TextChatService.ChatVersion == Enum.ChatVersion.TextChatService then return end
    local chatbox
    local success, result = pcall(function() chatbox = game.WaitForChild(PlayerGui, "Chat").Frame.ChatBarParentFrame.Frame.BoxFrame.Frame.ChatBar end)
    if success then
      local function chatboxFocused()
        canvasPos = CMDsF.CanvasPosition
      end
      local chatboxFocusedC = chatbox.Focused:Connect(chatboxFocused)
  
      local function Index()
        if chatbox.Text:lower():sub(1,1) == prefix then
          if SettingsOpen == true then
            wait(0.2)
            CMDsF.Visible = true
            Settings:TweenPosition(UDim2.new(0, 0, 0, 220), "InOut", "Quart", 0.2, true, nil)
          end
          IndexContents(PlayerGui.Chat.Frame.ChatBarParentFrame.Frame.BoxFrame.Frame.ChatBar.Text:lower():sub(2),true)
        else
          minimizeHolder()
          if SettingsOpen == true then
            wait(0.2)
            Settings:TweenPosition(UDim2.new(0, 0, 0, 45), "InOut", "Quart", 0.2, true, nil)
            CMDsF.Visible = false
          end
        end
      end
      local chatboxFunc = chatbox:GetPropertyChangedSignal("Text"):Connect(Index)
  
      local function chatboxFocusLost(enterpressed)
        if not enterpressed or chatbox.Text:lower():sub(1,1) ~= prefix then
          IndexContents('',true)
        end
        CMDsF.CanvasPosition = canvasPos
        minimizeHolder()
      end
      local chatboxFocusLostC = chatbox.FocusLost:Connect(chatboxFocusLost)
  
      PlayerGui:WaitForChild("Chat").Frame.ChatBarParentFrame.ChildAdded:Connect(function(newbar)
        wait()
        if newbar:FindFirstChild('BoxFrame') then
          chatbox = PlayerGui:WaitForChild("Chat").Frame.ChatBarParentFrame.Frame.BoxFrame.Frame.ChatBar
          if chatboxFocusedC then chatboxFocusedC:Disconnect() end
          chatboxFocusedC = chatbox.Focused:Connect(chatboxFocused)
          if chatboxFunc then chatboxFunc:Disconnect() end
          chatboxFunc = chatbox:GetPropertyChangedSignal("Text"):Connect(Index)
          if chatboxFocusLostC then chatboxFocusLostC:Disconnect() end
          chatboxFocusLostC = chatbox.FocusLost:Connect(chatboxFocusLost)
        end
      end)
      --else
      --print('Custom chat detected. Will not provide suggestions for commands typed in the chat.')
    end
  end)
  
  function autoComplete(str,curText)
    local endingChar = {"[", "/", "(", " "}
    local stop = 0
    for i=1,#str do
      local c = str:sub(i,i)
      if table.find(endingChar, c) then
        stop = i
        break
      end
    end
    curText = curText or Cmdbar.Text
    local subPos = 0
    local pos = 1
    local findRes = string.find(curText,"\\",pos)
    while findRes do
      subPos = findRes
      pos = findRes+1
      findRes = string.find(curText,"\\",pos)
    end
    if curText:sub(subPos+1,subPos+1) == "!" then subPos = subPos + 1 end
    Cmdbar.Text = curText:sub(1,subPos) .. str:sub(1, stop - 1)..' '
    wait()
    Cmdbar.Text = Cmdbar.Text:gsub( '\t', '' )
    Cmdbar.CursorPosition = #Cmdbar.Text+1--1020
  end
  
  CMDs = {}
  CMDs[#CMDs + 1] = {NAME = 'discord / support / help', DESC = 'Invite to the Infinite Yield support server.'}
  CMDs[#CMDs + 1] = {NAME = 'console', DESC = 'Loads old Roblox console'}
  CMDs[#CMDs + 1] = {NAME = 'explorer / dex', DESC = 'Opens DEX by Moon'}
  CMDs[#CMDs + 1] = {NAME = 'olddex / odex', DESC = 'Opens Old DEX by Moon'}
  CMDs[#CMDs + 1] = {NAME = 'remotespy / rspy', DESC = 'Opens Simple Spy V3'}
  CMDs[#CMDs + 1] = {NAME = 'audiologger / alogger', DESC = 'Opens Edges audio logger'}
  CMDs[#CMDs + 1] = {NAME = 'serverinfo / info', DESC = 'Gives you info about the server'}
  CMDs[#CMDs + 1] = {NAME = 'jobid', DESC = 'Copies the games JobId to your clipboard'}
  CMDs[#CMDs + 1] = {NAME = 'notifyjobid', DESC = 'Notifies you the games JobId'}
  CMDs[#CMDs + 1] = {NAME = 'rejoin / rj', DESC = 'Makes you rejoin the game'}
  CMDs[#CMDs + 1] = {NAME = 'autorejoin / autorj', DESC = 'Automatically rejoins the server if you get kicked/disconnected'}
  CMDs[#CMDs + 1] = {NAME = 'serverhop / shop', DESC = 'Teleports you to a different server'}
  CMDs[#CMDs + 1] = {NAME = 'joinplayer [username / ID] [place ID]', DESC = 'Joins a specific players server'}
  CMDs[#CMDs + 1] = {NAME = 'gameteleport / gametp [place ID]', DESC = 'Joins a game by ID'}
  CMDs[#CMDs + 1] = {NAME = 'antiidle / antiafk', DESC = 'Prevents the game from kicking you for being idle/afk'}
  CMDs[#CMDs + 1] = {NAME = 'datalimit [num]', DESC = 'Set outgoing KBPS limit'}
  CMDs[#CMDs + 1] = {NAME = 'replicationlag / backtrack [num]', DESC = 'Set IncomingReplicationLag'}
  CMDs[#CMDs + 1] = {NAME = 'creatorid / creator', DESC = 'Notifies you the creators ID'}
  CMDs[#CMDs + 1] = {NAME = 'copycreatorid / copycreator', DESC = 'Copies the creators ID to your clipboard'}
  CMDs[#CMDs + 1] = {NAME = 'setcreatorid / setcreator', DESC = 'Sets your userid to the creators ID'}
  CMDs[#CMDs + 1] = {NAME = 'noprompts', DESC = 'Prevents the game from showing you purchase/premium prompts'}
  CMDs[#CMDs + 1] = {NAME = 'showprompts', DESC = 'Allows the game to show purchase/premium prompts again'}
  CMDs[#CMDs + 1] = {NAME = 'enable [inventory/playerlist/chat/reset/emotes/all]', DESC = 'Toggles visibility of coregui items'}
  CMDs[#CMDs + 1] = {NAME = 'disable [inventory/playerlist/chat/reset/emotes/all]', DESC = 'Toggles visibility of coregui items'}
  CMDs[#CMDs + 1] = {NAME = 'showguis', DESC = 'Shows any invisible GUIs'}
  CMDs[#CMDs + 1] = {NAME = 'unshowguis', DESC = 'Undoes showguis'}
  CMDs[#CMDs + 1] = {NAME = 'hideguis', DESC = 'Hides any GUIs in PlayerGui'}
  CMDs[#CMDs + 1] = {NAME = 'unhideguis', DESC = 'Undoes hideguis'}
  CMDs[#CMDs + 1] = {NAME = 'guidelete', DESC = 'Enables backspace to delete GUI'}
  CMDs[#CMDs + 1] = {NAME = 'unguidelete / noguidelete', DESC = 'Disables guidelete'}
  CMDs[#CMDs + 1] = {NAME = 'hideiy', DESC = 'Hides the main IY GUI'}
  CMDs[#CMDs + 1] = {NAME = 'showiy / unhideiy', DESC = 'Shows IY again'}
  CMDs[#CMDs + 1] = {NAME = 'keepiy', DESC = 'Auto execute IY when you teleport through servers'}
  CMDs[#CMDs + 1] = {NAME = 'unkeepiy', DESC = 'Disable keepiy'}
  CMDs[#CMDs + 1] = {NAME = 'togglekeepiy', DESC = 'Toggle keepiy'}
  CMDs[#CMDs + 1] = {NAME = 'savegame / saveplace', DESC = 'Uses saveinstance to save the game'}
  CMDs[#CMDs + 1] = {NAME = 'clearerror', DESC = 'Clears the annoying box and blur when a game kicks you'}
  CMDs[#CMDs + 1] = {NAME = 'clientantikick / antikick (CLIENT)', DESC = 'Prevents localscripts from kicking you'}
  CMDs[#CMDs + 1] = {NAME = 'clientantiteleport / antiteleport (CLIENT)', DESC = 'Prevents localscripts from teleporting you'}
  CMDs[#CMDs + 1] = {NAME = 'allowrejoin / allowrj [true/false] (CLIENT)', DESC = 'Changes if antiteleport allows you to rejoin or not'}
  CMDs[#CMDs + 1] = {NAME = 'cancelteleport / canceltp', DESC = 'Cancels teleports in progress'}
  CMDs[#CMDs + 1] = {NAME = 'volume / vol [0-10]', DESC = 'Adjusts your game volume on a scale of 0 to 10'}
  CMDs[#CMDs + 1] = {NAME = 'antilag / boostfps / lowgraphics', DESC = 'Lowers game quality to boost FPS'}
  CMDs[#CMDs + 1] = {NAME = 'record / rec', DESC = 'Starts roblox recorder'}
  CMDs[#CMDs + 1] = {NAME = 'screenshot / scrnshot', DESC = 'Takes a screenshot'}
  CMDs[#CMDs + 1] = {NAME = 'togglefullscreen / togglefs', DESC = 'Toggles fullscreen'}
  CMDs[#CMDs + 1] = {NAME = 'notify [text]', DESC = 'Sends you a notification with the provided text'}
  CMDs[#CMDs + 1] = {NAME = 'lastcommand / lastcmd', DESC = 'Executes the previous command used'}
  CMDs[#CMDs + 1] = {NAME = 'exit', DESC = 'Kills roblox process'}
  CMDs[#CMDs + 1] = {NAME = '', DESC = ''}
  CMDs[#CMDs + 1] = {NAME = 'noclip', DESC = 'Go through objects'}
  CMDs[#CMDs + 1] = {NAME = 'unnoclip / clip', DESC = 'Disables noclip'}
  CMDs[#CMDs + 1] = {NAME = 'fly [speed]', DESC = 'Makes you fly'}
  CMDs[#CMDs + 1] = {NAME = 'unfly', DESC = 'Disables fly'}
  CMDs[#CMDs + 1] = {NAME = 'flyspeed [num]', DESC = 'Set fly speed (default is 20)'}
  CMDs[#CMDs + 1] = {NAME = 'vehiclefly / vfly [speed]', DESC = 'Makes you fly in a vehicle'}
  CMDs[#CMDs + 1] = {NAME = 'unvehiclefly / unvfly', DESC = 'Disables vehicle fly'}
  CMDs[#CMDs + 1] = {NAME = 'vehicleflyspeed  / vflyspeed [num]', DESC = 'Set vehicle fly speed'}
  CMDs[#CMDs + 1] = {NAME = 'cframefly / cfly [speed]', DESC = 'Makes you fly, bypassing some anti cheats (works on mobile)'}
  CMDs[#CMDs + 1] = {NAME = 'uncframefly / uncfly', DESC = 'Disables cfly'}
  CMDs[#CMDs + 1] = {NAME = 'cframeflyspeed  / cflyspeed [num]', DESC = 'Sets cfly speed'}
  CMDs[#CMDs + 1] = {NAME = 'qefly [true / false]', DESC = 'enables or disables the Q and E hotkeys for fly'}
  CMDs[#CMDs + 1] = {NAME = 'vehiclenoclip / vnoclip', DESC = 'Turns off vehicle collision'}
  CMDs[#CMDs + 1] = {NAME = 'vehicleclip / vclip / unvnoclip', DESC = 'Enables vehicle collision'}
  CMDs[#CMDs + 1] = {NAME = 'float /  platform', DESC = 'Spawns a platform beneath you causing you to float'}
  CMDs[#CMDs + 1] = {NAME = 'unfloat / noplatform', DESC = 'Removes the platform'}
  CMDs[#CMDs + 1] = {NAME = 'swim', DESC = 'Allows you to swim in the air'}
  CMDs[#CMDs + 1] = {NAME = 'unswim / noswim', DESC = 'Stops you from swimming everywhere'}
  CMDs[#CMDs + 1] = {NAME = '', DESC = ''}
  CMDs[#CMDs + 1] = {NAME = 'setwaypoint / swp [name]', DESC = 'Sets a waypoint at your position'}
  CMDs[#CMDs + 1] = {NAME = 'waypointpos / wpp [name] [X Y Z]', DESC = 'Sets a waypoint with specified coordinates'}
  CMDs[#CMDs + 1] = {NAME = 'waypoints', DESC = 'Shows a list of currently active waypoints'}
  CMDs[#CMDs + 1] = {NAME = 'showwaypoints / showwp', DESC = 'Shows all currently set waypoints'}
  CMDs[#CMDs + 1] = {NAME = 'hidewaypoints / hidewp', DESC = 'Hides shown waypoints'}
  CMDs[#CMDs + 1] = {NAME = 'waypoint / wp [name]', DESC = 'Teleports player to a waypoint'}
  CMDs[#CMDs + 1] = {NAME = 'tweenwaypoint / twp [name]', DESC = 'Tweens player to a waypoint'}
  CMDs[#CMDs + 1] = {NAME = 'walktowaypoint / wtwp [name]', DESC = 'Walks player to a waypoint'}
  CMDs[#CMDs + 1] = {NAME = 'deletewaypoint / dwp [name]', DESC = 'Deletes a waypoint'}
  CMDs[#CMDs + 1] = {NAME = 'clearwaypoints / cwp', DESC = 'Clears all waypoints'}
  CMDs[#CMDs + 1] = {NAME = 'cleargamewaypoints / cgamewp', DESC = 'Clears all waypoints for the game you are in'}
  CMDs[#CMDs + 1] = {NAME = '', DESC = ''}
  CMDs[#CMDs + 1] = {NAME = 'goto [player]', DESC = 'Go to a player'}
  CMDs[#CMDs + 1] = {NAME = 'tweengoto / tgoto [player]', DESC = 'Tween to a player (bypasses some anti cheats)'}
  CMDs[#CMDs + 1] = {NAME = 'tweenspeed / tspeed [num]', DESC = 'Sets how fast all tween commands go (default is 1)'}
  CMDs[#CMDs + 1] = {NAME = 'vehiclegoto / vgoto [player]', DESC = 'Go to a player while in a vehicle'}
  CMDs[#CMDs + 1] = {NAME = 'loopgoto [player] [distance] [delay]', DESC = 'Loop teleport to a player'}
  CMDs[#CMDs + 1] = {NAME = 'unloopgoto', DESC = 'Stops teleporting you to a player'}
  CMDs[#CMDs + 1] = {NAME = 'pulsetp / ptp [player] [seconds]', DESC = 'Teleports you to a player for a specified ammount of time'}
  CMDs[#CMDs + 1] = {NAME = 'clientbring / cbring [player] (CLIENT)', DESC = 'Bring a player'}
  CMDs[#CMDs + 1] = {NAME = 'loopbring [player] [distance] [delay] (CLIENT)', DESC = 'Loop brings a player to you (useful for killing)'}
  CMDs[#CMDs + 1] = {NAME = 'unloopbring [player]', DESC = 'Undoes loopbring'}
  CMDs[#CMDs + 1] = {NAME = 'freeze / fr [player] (CLIENT)', DESC = 'Freezes a player'}
  CMDs[#CMDs + 1] = {NAME = 'freezeanims', DESC = 'Freezes your animations / pauses your animations - Does not work on default animations'}
  CMDs[#CMDs + 1] = {NAME = 'unfreezeanims', DESC = 'Unfreezes your animations / plays your animations'}
  CMDs[#CMDs + 1] = {NAME = 'thaw / unfr [player] (CLIENT)', DESC = 'Unfreezes a player'}
  CMDs[#CMDs + 1] = {NAME = 'tpposition / tppos [X Y Z]', DESC = 'Teleports you to certain coordinates'}
  CMDs[#CMDs + 1] = {NAME = 'tweentpposition / ttppos [X Y Z]', DESC = 'Tween to coordinates (bypasses some anti cheats)'}
  CMDs[#CMDs + 1] = {NAME = 'offset [X Y Z]', DESC = 'Offsets you by certain coordinates'}
  CMDs[#CMDs + 1] = {NAME = 'tweenoffset / toffset [X Y Z]', DESC = 'Tween offset (bypasses some anti cheats)'}
  CMDs[#CMDs + 1] = {NAME = 'notifyposition / notifypos [player]', DESC = 'Notifies you the coordinates of a character'}
  CMDs[#CMDs + 1] = {NAME = 'copyposition / copypos [player]', DESC = 'Copies the coordinates of a character to your clipboard'}
  CMDs[#CMDs + 1] = {NAME = 'walktoposition / walktopos [X Y Z]', DESC = 'Makes you walk to a coordinate'}
  CMDs[#CMDs + 1] = {NAME = 'spawnpoint / spawn [delay]', DESC = 'Sets a position where you will spawn'}
  CMDs[#CMDs + 1] = {NAME = 'nospawnpoint / nospawn', DESC = 'Removes your custom spawn point'}
  CMDs[#CMDs + 1] = {NAME = 'flashback / diedtp', DESC = 'Teleports you to where you last died'}
  CMDs[#CMDs + 1] = {NAME = 'walltp', DESC = 'Teleports you above/over any wall you run into'}
  CMDs[#CMDs + 1] = {NAME = 'nowalltp / unwalltp', DESC = 'Disables walltp'}
  CMDs[#CMDs + 1] = {NAME = 'teleporttool / tptool', DESC = 'Gives you a teleport tool'}
  CMDs[#CMDs + 1] = {NAME = '', DESC = ''}
  CMDs[#CMDs + 1] = {NAME = 'logs', DESC = 'Opens the logs GUI'}
  CMDs[#CMDs + 1] = {NAME = 'chatlogs / clogs', DESC = 'Log what people say or whisper'}
  CMDs[#CMDs + 1] = {NAME = 'joinlogs / jlogs', DESC = 'Log when people join'}
  CMDs[#CMDs + 1] = {NAME = 'chat / say [text]', DESC = 'Makes you chat a string (possible mute bypass)'}
  CMDs[#CMDs + 1] = {NAME = 'spam [text]', DESC = 'Makes you spam the chat'}
  CMDs[#CMDs + 1] = {NAME = 'unspam', DESC = 'Turns off spam'}
  CMDs[#CMDs + 1] = {NAME = 'whisper / pm [player] [text]', DESC = 'Makes you whisper a string to someone (possible mute bypass)'}
  CMDs[#CMDs + 1] = {NAME = 'pmspam [player] [text]', DESC = 'Makes you spam a players whispers'}
  CMDs[#CMDs + 1] = {NAME = 'unpmspam [player]', DESC = 'Turns off pm spam'}
  CMDs[#CMDs + 1] = {NAME = 'spamspeed [num]', DESC = 'How quickly you spam (default is 1)'}
  CMDs[#CMDs + 1] = {NAME = 'bubblechat (CLIENT)', DESC = 'Enables bubble chat for your client'}
  CMDs[#CMDs + 1] = {NAME = 'unbubblechat / nobubblechat', DESC = 'Disables the bubblechat command'}
  CMDs[#CMDs + 1] = {NAME = 'safechat', DESC = 'Enables safe chat'}
  CMDs[#CMDs + 1] = {NAME = 'nosafechat / disablesafechat', DESC = 'Disables safechat'}
  CMDs[#CMDs + 1] = {NAME = '', DESC = ''}
  CMDs[#CMDs + 1] = {NAME = 'esp', DESC = 'View all players and their status'}
  CMDs[#CMDs + 1] = {NAME = 'noesp / unesp', DESC = 'Removes esp'}
  CMDs[#CMDs + 1] = {NAME = 'esptransparency [number]', DESC = 'Changes the transparency of esp related commands'}
  CMDs[#CMDs + 1] = {NAME = 'partesp [part name]', DESC = 'Highlights a part'}
  CMDs[#CMDs + 1] = {NAME = 'unpartesp / nopartesp [part name]', DESC = 'removes partesp'}
  CMDs[#CMDs + 1] = {NAME = 'chams', DESC = 'ESP but without text in the way'}
  CMDs[#CMDs + 1] = {NAME = 'nochams / unchams', DESC = 'Removes chams'}
  CMDs[#CMDs + 1] = {NAME = 'locate [player]', DESC = 'View a single player and their status'}
  CMDs[#CMDs + 1] = {NAME = 'unlocate / nolocate [player]', DESC = 'Removes locate'}
  CMDs[#CMDs + 1] = {NAME = 'xray', DESC = 'Makes all parts in workspace transparent'}
  CMDs[#CMDs + 1] = {NAME = 'unxray / noxray', DESC = 'Restores transparency'}
  CMDs[#CMDs + 1] = {NAME = 'loopxray', DESC = 'Makes all parts in workspace transparent but looped'}
  CMDs[#CMDs + 1] = {NAME = 'unloopunxray', DESC = 'Unloops xray'}
  CMDs[#CMDs + 1] = {NAME = '', DESC = ''}
  CMDs[#CMDs + 1] = {NAME = 'spectate / view [player]', DESC = 'View a player'}
  CMDs[#CMDs + 1] = {NAME = 'viewpart / viewp [part name]', DESC = 'View a part'}
  CMDs[#CMDs + 1] = {NAME = 'unspectate / unview', DESC = 'Stops viewing player'}
  CMDs[#CMDs + 1] = {NAME = 'freecam / fc', DESC = 'Allows you to freely move camera around the game'}
  CMDs[#CMDs + 1] = {NAME = 'freecampos / fcpos [X Y Z]', DESC = 'Moves / opens freecam in a certain position'}
  CMDs[#CMDs + 1] = {NAME = 'freecamwaypoint / fcwp [name]', DESC = 'Moves / opens freecam to a waypoint'}
  CMDs[#CMDs + 1] = {NAME = 'freecamgoto / fcgoto / fctp [player]', DESC = 'Moves / opens freecam to a player'}
  CMDs[#CMDs + 1] = {NAME = 'unfreecam / unfc', DESC = 'Disables freecam'}
  CMDs[#CMDs + 1] = {NAME = 'freecamspeed / fcspeed [num]', DESC = 'Adjusts freecam speed (default is 1)'}
  CMDs[#CMDs + 1] = {NAME = 'notifyfreecamposition / notifyfcpos', DESC = 'Noitifies you your freecam coordinates'}
  CMDs[#CMDs + 1] = {NAME = 'copyfreecamposition / copyfcpos', DESC = 'Copies your freecam coordinates to your clipboard'}
  CMDs[#CMDs + 1] = {NAME = 'gotocamera / gotocam', DESC = 'Teleports you to the location of your camera'}
  CMDs[#CMDs + 1] = {NAME = 'tweengotocam / tgotocam', DESC = 'Tweens you to the location of your camera'}
  CMDs[#CMDs + 1] = {NAME = 'firstp', DESC = 'Forces camera to go into first person'}
  CMDs[#CMDs + 1] = {NAME = 'thirdp', DESC = 'Allows camera to go into third person'}
  CMDs[#CMDs + 1] = {NAME = 'noclipcam / nccam', DESC = 'Allows camera to go through objects like walls'}
  CMDs[#CMDs + 1] = {NAME = 'maxzoom [num]', DESC = 'Maximum camera zoom'}
  CMDs[#CMDs + 1] = {NAME = 'minzoom [num]', DESC = 'Minimum camera zoom'}
  CMDs[#CMDs + 1] = {NAME = 'camdistance [num]', DESC = 'Changes camera distance from your player'}
  CMDs[#CMDs + 1] = {NAME = 'fov [num]', DESC = 'Adjusts field of view (default is 70)'}
  CMDs[#CMDs + 1] = {NAME = 'fixcam / restorecam', DESC = 'Fixes camera'}
  CMDs[#CMDs + 1] = {NAME = 'enableshiftlock / enablesl', DESC = 'Enables the shift lock option'}
  CMDs[#CMDs + 1] = {NAME = 'lookat [player]', DESC = 'Moves your camera view to a player'}
  CMDs[#CMDs + 1] = {NAME = '', DESC = ''}
  CMDs[#CMDs + 1] = {NAME = 'btools (CLIENT)', DESC = 'Gives you building tools (DOES NOT REPLICATE)'}
  CMDs[#CMDs + 1] = {NAME = 'f3x (CLIENT)', DESC = 'Gives you F3X building tools (DOES NOT REPLICATE)'}
  CMDs[#CMDs + 1] = {NAME = 'partname / partpath', DESC = 'Allows you to click a part to see its path & name'}
  CMDs[#CMDs + 1] = {NAME = 'delete [instance name] (CLIENT)', DESC = 'Removes any part with a certain name from the workspace (DOES NOT REPLICATE)'}
  CMDs[#CMDs + 1] = {NAME = 'deleteclass / dc [class name] (CLIENT)', DESC = 'Removes any part with a certain classname from the workspace (DOES NOT REPLICATE)'}
  CMDs[#CMDs + 1] = {NAME = 'lockworkspace / lockws', DESC = 'Locks the whole workspace'}
  CMDs[#CMDs + 1] = {NAME = 'unlockworkspace / unlockws', DESC = 'Unlocks the whole workspace'}
  CMDs[#CMDs + 1] = {NAME = 'invisibleparts / invisparts (CLIENT)', DESC = 'Shows invisible parts'}
  CMDs[#CMDs + 1] = {NAME = 'uninvisibleparts / uninvisparts (CLIENT)', DESC = 'Makes parts affected by invisparts return to normal'}
  CMDs[#CMDs + 1] = {NAME = 'deleteinvisparts / dip (CLIENT)', DESC = 'Deletes invisible parts'}
  CMDs[#CMDs + 1] = {NAME = 'gotopart [part name]', DESC = 'Moves your character to a part or multiple parts'}
  CMDs[#CMDs + 1] = {NAME = 'tweengotopart / tgotopart [part name]', DESC = 'Tweens your character to a part or multiple parts'}
  CMDs[#CMDs + 1] = {NAME = 'gotopartclass / gpc [class name]', DESC = 'Moves your character to a part or multiple parts based on classname'}
  CMDs[#CMDs + 1] = {NAME = 'tweengotopartclass / tgpc [class name]', DESC = 'Tweens your character to a part or multiple parts based on classname'}
  CMDs[#CMDs + 1] = {NAME = 'gotomodel [part name]', DESC = 'Moves your character to a model or multiple models'}
  CMDs[#CMDs + 1] = {NAME = 'tweengotomodel / tgotomodel [part name]', DESC = 'Tweens your character to a model or multiple models'}
  CMDs[#CMDs + 1] = {NAME = 'gotopartdelay / gotomodeldelay [num]', DESC = 'Adjusts how quickly you teleport to each part (default is 0.1)'}
  CMDs[#CMDs + 1] = {NAME = 'bringpart [part name] (CLIENT)', DESC = 'Moves a part or multiple parts to your character'}
  CMDs[#CMDs + 1] = {NAME = 'bringpartclass / bpc [class name] (CLIENT)', DESC = 'Moves a part or multiple parts to your character based on classname'}
  CMDs[#CMDs + 1] = {NAME = 'noclickdetectorlimits / nocdlimits', DESC = 'Sets all click detectors MaxActivationDistance to math.huge'}
  CMDs[#CMDs + 1] = {NAME = 'fireclickdetectors / firecd [name]', DESC = 'Uses all click detectors in a game or uses the optional name'}
  CMDs[#CMDs + 1] = {NAME = 'firetouchinterests / touchinterests [name]', DESC = 'Uses all touchinterests in a game or uses the optional name'}
  CMDs[#CMDs + 1] = {NAME = 'noproximitypromptlimits / nopplimits', DESC = 'Sets all proximity prompts MaxActivationDistance to math.huge'}
  CMDs[#CMDs + 1] = {NAME = 'fireproximityprompts / firepp [name]', DESC = 'Uses all proximity prompts in a game or uses the optional name'}
  CMDs[#CMDs + 1] = {NAME = 'instantproximityprompts / instantpp', DESC = 'Disable the cooldown for proximity prompts'}
  CMDs[#CMDs + 1] = {NAME = 'uninstantproximityprompts / uninstantpp', DESC = 'Undo the cooldown removal'}
  CMDs[#CMDs + 1] = {NAME = 'tpunanchored / tpua [player]', DESC = 'Teleports unanchored parts to a player'}
  CMDs[#CMDs + 1] = {NAME = 'animsunanchored / freezeua', DESC = 'Freezes unanchored parts'}
  CMDs[#CMDs + 1] = {NAME = 'thawunanchored / thawua / unfreezeua', DESC = 'Thaws unanchored parts'}
  CMDs[#CMDs + 1] = {NAME = 'removeterrain / rterrain / noterrain', DESC = 'Removes all terrain'}
  CMDs[#CMDs + 1] = {NAME = 'clearnilinstances / nonilinstances / cni', DESC = 'Removes nil instances'}
  CMDs[#CMDs + 1] = {NAME = 'destroyheight / dh [num]', DESC = 'Sets FallenPartsDestroyHeight'}
  CMDs[#CMDs + 1] = {NAME = '', DESC = ''}
  CMDs[#CMDs + 1] = {NAME = 'fullbright / fb (CLIENT)', DESC = 'Makes the map brighter / more visible'}
  CMDs[#CMDs + 1] = {NAME = 'loopfullbright / loopfb (CLIENT)', DESC = 'Makes the map brighter / more visible but looped'}
  CMDs[#CMDs + 1] = {NAME = 'unloopfullbright / unloopfb', DESC = 'Unloops fullbright'}
  CMDs[#CMDs + 1] = {NAME = 'ambient [num] [num] [num] (CLIENT)', DESC = 'Changes ambient'}
  CMDs[#CMDs + 1] = {NAME = 'day (CLIENT)', DESC = 'Changes the time to day for the client'}
  CMDs[#CMDs + 1] = {NAME = 'night (CLIENT)', DESC = 'Changes the time to night for the client'}
  CMDs[#CMDs + 1] = {NAME = 'nofog (CLIENT)', DESC = 'Removes fog'}
  CMDs[#CMDs + 1] = {NAME = 'brightness [num] (CLIENT)', DESC = 'Changes the brightness lighting property'}
  CMDs[#CMDs + 1] = {NAME = 'globalshadows / gshadows (CLIENT)', DESC = 'Enables global shadows'}
  CMDs[#CMDs + 1] = {NAME = 'noglobalshadows / nogshadows (CLIENT)', DESC = 'Disables global shadows'}
  CMDs[#CMDs + 1] = {NAME = 'restorelighting / rlighting', DESC = 'Restores Lighting properties'}
  CMDs[#CMDs + 1] = {NAME = 'light [radius] [brightness] (CLIENT)', DESC = 'Gives your player dynamic light'}
  CMDs[#CMDs + 1] = {NAME = 'nolight / unlight', DESC = 'Removes dynamic light from your player'}
  CMDs[#CMDs + 1] = {NAME = '', DESC = ''}
  CMDs[#CMDs + 1] = {NAME = 'inspect / examine [player]', DESC = 'Opens InspectMenu for a certain player'}
  CMDs[#CMDs + 1] = {NAME = 'age [player]', DESC = 'Tells you the age of a player'}
  CMDs[#CMDs + 1] = {NAME = 'chatage [player]', DESC = 'Chats the age of a player'}
  CMDs[#CMDs + 1] = {NAME = 'joindate / jd [player]', DESC = 'Tells you the date the player joined Roblox'}
  CMDs[#CMDs + 1] = {NAME = 'chatjoindate / cjd [player]', DESC = 'Chats the date the player joined Roblox'}
  CMDs[#CMDs + 1] = {NAME = 'copyname / copyuser [player]', DESC = 'Copies a players full username to your clipboard'}
  CMDs[#CMDs + 1] = {NAME = 'userid / id [player]', DESC = 'Notifies a players user ID'}
  CMDs[#CMDs + 1] = {NAME = 'copyuserid / copyid [player]', DESC = 'Copies a players user ID to your clipboard'}
  CMDs[#CMDs + 1] = {NAME = 'appearanceid / aid [player]', DESC = 'Notifies a players appearance ID'}
  CMDs[#CMDs + 1] = {NAME = 'copyappearanceid / caid [player]', DESC = 'Copies a players appearance ID to your clipboard'}
  CMDs[#CMDs + 1] = {NAME = 'bang [player] [speed]', DESC = 'owo'}
  CMDs[#CMDs + 1] = {NAME = 'unbang', DESC = 'uwu'}
  CMDs[#CMDs + 1] = {NAME = 'carpet [player]', DESC = 'Be someones carpet'}
  CMDs[#CMDs + 1] = {NAME = 'uncarpet', DESC = 'Undoes carpet'}
  CMDs[#CMDs + 1] = {NAME = 'friend [player]', DESC = 'Sends a friend request to certain players'}
  CMDs[#CMDs + 1] = {NAME = 'unfriend [player]', DESC = 'Unfriends certain players'}
  CMDs[#CMDs + 1] = {NAME = 'headsit [player]', DESC = 'Sit on a players head'}
  CMDs[#CMDs + 1] = {NAME = 'walkto / follow [player]', DESC = 'Follow a player'}
  CMDs[#CMDs + 1] = {NAME = 'pathfindwalkto / pathfindfollow [player]', DESC = 'Follow a player using pathfinding'}
  CMDs[#CMDs + 1] = {NAME = 'pathfindwalktowaypoint / pathfindwalktowp [waypoint]', DESC = 'Walk to a waypoint using pathfinding'}
  CMDs[#CMDs + 1] = {NAME = 'unwalkto / unfollow', DESC = 'Stops following a player'}
  CMDs[#CMDs + 1] = {NAME = 'stareat / stare [player]', DESC = 'Stare / look at a player'}
  CMDs[#CMDs + 1] = {NAME = 'unstareat / unstare [player]', DESC = 'Disables stareat'}
  CMDs[#CMDs + 1] = {NAME = 'rolewatch [group id] [role name]', DESC = 'Notify if someone from a watched group joins the server'}
  CMDs[#CMDs + 1] = {NAME = 'rolewatchstop / unrolewatch', DESC = 'Disable Rolewatch'}
  CMDs[#CMDs + 1] = {NAME = 'rolewatchleave', DESC = 'Toggle if you should leave the game if someone from a watched group joins the server'}
  CMDs[#CMDs + 1] = {NAME = 'staffwatch', DESC = 'Notify if a staff member of the game joins the server'}
  CMDs[#CMDs + 1] = {NAME = 'unstaffwatch', DESC = 'Disable Staffwatch'}
  CMDs[#CMDs + 1] = {NAME = 'attach [player] (TOOL)', DESC = 'Attaches you to a player (YOU NEED A TOOL)'}
  CMDs[#CMDs + 1] = {NAME = 'kill [player] (TOOL)', DESC = 'Kills a player (YOU NEED A TOOL)'}
  CMDs[#CMDs + 1] = {NAME = 'fastkill [player] (TOOL)', DESC = 'Kills a player (less reliable) (YOU NEED A TOOL)'}
  CMDs[#CMDs + 1] = {NAME = 'handlekill / hkill [player] (TOOL)', DESC = 'Kills a player using tool damage (YOU NEED A TOOL)'}
  CMDs[#CMDs + 1] = {NAME = 'bring [player] (TOOL)', DESC = 'Brings a player (YOU NEED A TOOL)'}
  CMDs[#CMDs + 1] = {NAME = 'fastbring [player] (TOOL)', DESC = 'Brings a player (less reliable) (YOU NEED A TOOL)'}
  CMDs[#CMDs + 1] = {NAME = 'teleport / tp [player] [player] (TOOL)', DESC = 'Teleports a player to another player (YOU NEED A TOOL)'}
  CMDs[#CMDs + 1] = {NAME = 'fastteleport / fasttp [player] [player] (TOOL)', DESC = 'Teleports a player to another player (less reliable) (YOU NEED A TOOL)'}
  CMDs[#CMDs + 1] = {NAME = 'fling', DESC = 'Flings anyone you touch'}
  CMDs[#CMDs + 1] = {NAME = 'unfling', DESC = 'Disables the fling command'}
  CMDs[#CMDs + 1] = {NAME = 'flyfling', DESC = 'Basically the invisfling command but not invisible'}
  CMDs[#CMDs + 1] = {NAME = 'unflyfling', DESC = 'Disables the flyfling command'}
  CMDs[#CMDs + 1] = {NAME = 'invisfling', DESC = 'Enables invisible fling'}
  CMDs[#CMDs + 1] = {NAME = 'loopoof', DESC = 'Loops everyones character sounds (everyone can hear)'}
  CMDs[#CMDs + 1] = {NAME = 'unloopoof', DESC = 'Stops the oof chaos'}
  CMDs[#CMDs + 1] = {NAME = 'muteboombox [player]', DESC = 'Mutes someones boombox'}
  CMDs[#CMDs + 1] = {NAME = 'unmuteboombox [player]', DESC = 'Unmutes someones boombox'}
  CMDs[#CMDs + 1] = {NAME = 'hitbox [player] [size]', DESC = 'Expands the hitbox for players HumanoidRootPart (default is 1)'}
  CMDs[#CMDs + 1] = {NAME = 'headsize [player] [size]', DESC = 'Expands the head size for players Head (default is 1)'}
  CMDs[#CMDs + 1] = {NAME = '', DESC = ''}
  CMDs[#CMDs + 1] = {NAME = 'reset', DESC = 'Resets your character normally'}
  CMDs[#CMDs + 1] = {NAME = 'respawn', DESC = 'Respawns you'}
  CMDs[#CMDs + 1] = {NAME = 'refresh / re', DESC = 'Respawns and brings you back to the same position'}
  CMDs[#CMDs + 1] = {NAME = 'god', DESC = 'Makes your character difficult to kill in most games'}
  CMDs[#CMDs + 1] = {NAME = 'invisible / invis', DESC = 'Makes you invisible to other players'}
  CMDs[#CMDs + 1] = {NAME = 'visible / vis', DESC = 'Makes you visible to other players'}
  CMDs[#CMDs + 1] = {NAME = 'toolinvisible / toolinvis / tinvis', DESC = 'Makes you invisible to other players and able to use tools'}
  CMDs[#CMDs + 1] = {NAME = 'speed / ws / walkspeed [num]', DESC = 'Change your walkspeed'}
  CMDs[#CMDs + 1] = {NAME = 'spoofspeed / spoofws [num]', DESC = 'Spoofs your WalkSpeed on the Client'}
  CMDs[#CMDs + 1] = {NAME = 'loopspeed / loopws [num]', DESC = 'Loops your walkspeed'}
  CMDs[#CMDs + 1] = {NAME = 'unloopspeed / unloopws', DESC = 'Turns off loopspeed'}
  CMDs[#CMDs + 1] = {NAME = 'hipheight / hheight [num]', DESC = 'Adjusts hip height'}
  CMDs[#CMDs + 1] = {NAME = 'jumppower / jpower / jp [num]', DESC = 'Change a players jump height'}
  CMDs[#CMDs + 1] = {NAME = 'spoofjumppower / spoofjp [num]', DESC = 'Spoofs your JumpPower on the Client'}
  CMDs[#CMDs + 1] = {NAME = 'loopjumppower / loopjp [num]', DESC = 'Loops your jump height'}
  CMDs[#CMDs + 1] = {NAME = 'unloopjumppower / unloopjp', DESC = 'Turns off loopjumppower'}
  CMDs[#CMDs + 1] = {NAME = 'maxslopeangle / msa [num]', DESC = 'Adjusts MaxSlopeAngle'}
  CMDs[#CMDs + 1] = {NAME = 'gravity / grav [num] (CLIENT)', DESC = 'Change your gravity'}
  CMDs[#CMDs + 1] = {NAME = 'sit', DESC = 'Makes your character sit'}
  CMDs[#CMDs + 1] = {NAME = 'lay / laydown', DESC = 'Makes your character lay down'}
  CMDs[#CMDs + 1] = {NAME = 'sitwalk', DESC = 'Makes your character sit while still being able to walk'}
  CMDs[#CMDs + 1] = {NAME = 'nosit', DESC = 'Prevents your character from sitting'}
  CMDs[#CMDs + 1] = {NAME = 'unnosit', DESC = 'Disables nosit'}
  CMDs[#CMDs + 1] = {NAME = 'jump', DESC = 'Makes your character jump'}
  CMDs[#CMDs + 1] = {NAME = 'infinitejump / infjump', DESC = 'Allows you to jump before hitting the ground'}
  CMDs[#CMDs + 1] = {NAME = 'uninfinitejump / uninfjump', DESC = 'Disables infjump'}
  CMDs[#CMDs + 1] = {NAME = 'flyjump', DESC = 'Allows you to hold space to fly up'}
  CMDs[#CMDs + 1] = {NAME = 'unflyjump', DESC = 'Disables flyjump'}
  CMDs[#CMDs + 1] = {NAME = 'autojump / ajump', DESC = 'Automatically jumps when you run into an object'}
  CMDs[#CMDs + 1] = {NAME = 'unautojump / unajump', DESC = 'Disables autojump'}
  CMDs[#CMDs + 1] = {NAME = 'edgejump / ejump', DESC = 'Automatically jumps when you get to the edge of an object'}
  CMDs[#CMDs + 1] = {NAME = 'unedgejump / unejump', DESC = 'Disables edgejump'}
  CMDs[#CMDs + 1] = {NAME = 'platformstand / stun', DESC = 'Enables PlatformStand'}
  CMDs[#CMDs + 1] = {NAME = 'unplatformstand / unstun', DESC = 'Disables PlatformStand'}
  CMDs[#CMDs + 1] = {NAME = 'norotate / noautorotate', DESC = 'Disables AutoRotate'}
  CMDs[#CMDs + 1] = {NAME = 'unnorotate / autorotate', DESC = 'Enables AutoRotate'}
  CMDs[#CMDs + 1] = {NAME = 'enablestate [StateType]', DESC = 'Enables a humanoid state type'}
  CMDs[#CMDs + 1] = {NAME = 'disablestate [StateType]', DESC = 'Disables a humanoid state type'}
  CMDs[#CMDs + 1] = {NAME = 'team [team name] (CLIENT)', DESC = 'Changes your team. Sometimes fools localscripts.'}
  CMDs[#CMDs + 1] = {NAME = 'nobillboardgui / nobgui / noname', DESC = 'Removes billboard and surface guis from your players (i.e. name guis at cafes)'}
  CMDs[#CMDs + 1] = {NAME = 'loopnobgui / loopnoname', DESC = 'Loop removes billboard and surface guis from your players (i.e. name guis at cafes)'}
  CMDs[#CMDs + 1] = {NAME = 'unloopnobgui / unloopnoname', DESC = 'Disables loopnobgui'}
  CMDs[#CMDs + 1] = {NAME = 'nohead / headless', DESC = 'Removes your head (uses simulation radius)'}
  CMDs[#CMDs + 1] = {NAME = 'noarms', DESC = 'Removes your arms'}
  CMDs[#CMDs + 1] = {NAME = 'nolegs', DESC = 'Removes your legs'}
  CMDs[#CMDs + 1] = {NAME = 'nolimbs', DESC = 'Removes your limbs'}
  CMDs[#CMDs + 1] = {NAME = 'naked (CLIENT)', DESC = 'Removes your clothing'}
  CMDs[#CMDs + 1] = {NAME = 'noface / removeface', DESC = 'Removes your face'}
  CMDs[#CMDs + 1] = {NAME = 'blockhead', DESC = 'Turns your head into a block'}
  CMDs[#CMDs + 1] = {NAME = 'blockhats', DESC = 'Turns your hats into blocks'}
  CMDs[#CMDs + 1] = {NAME = 'blocktool', DESC = 'Turns the currently selected tool into a block'}
  CMDs[#CMDs + 1] = {NAME = 'creeper', DESC = 'Makes you look like a creeper'}
  CMDs[#CMDs + 1] = {NAME = 'drophats', DESC = 'Drops your hats'}
  CMDs[#CMDs + 1] = {NAME = 'nohats / deletehats / rhats', DESC = 'Deletes your hats'}
  CMDs[#CMDs + 1] = {NAME = 'hatspin / spinhats', DESC = 'Spins your characters accessories'}
  CMDs[#CMDs + 1] = {NAME = 'unhatspin / unspinhats', DESC = 'Undoes spinhats'}
  CMDs[#CMDs + 1] = {NAME = 'clearhats / cleanhats', DESC = 'Clears hats in the workspace'}
  CMDs[#CMDs + 1] = {NAME = 'chardelete / cd [instance name]', DESC = 'Removes any part with a certain name from your character'}
  CMDs[#CMDs + 1] = {NAME = 'chardeleteclass / cdc [class name]', DESC = 'Removes any part with a certain classname from your character'}
  CMDs[#CMDs + 1] = {NAME = 'deletevelocity / dv / removeforces', DESC = 'Removes any velocity / force instances in your character'}
  CMDs[#CMDs + 1] = {NAME = 'weaken [num]', DESC = 'Makes your character less dense'}
  CMDs[#CMDs + 1] = {NAME = 'unweaken', DESC = 'Sets your characters CustomPhysicalProperties to default'}
  CMDs[#CMDs + 1] = {NAME = 'strengthen [num]', DESC = 'Makes your character more dense (CustomPhysicalProperties)'}
  CMDs[#CMDs + 1] = {NAME = 'unstrengthen', DESC = 'Sets your characters CustomPhysicalProperties to default'}
  CMDs[#CMDs + 1] = {NAME = 'breakvelocity', DESC = 'Sets your characters velocity to 0'}
  CMDs[#CMDs + 1] = {NAME = 'spin [speed]', DESC = 'Spins your character'}
  CMDs[#CMDs + 1] = {NAME = 'unspin', DESC = 'Disables spin'}
  CMDs[#CMDs + 1] = {NAME = 'vr', DESC = 'Loads CLOVR by Abacaxl'}
  CMDs[#CMDs + 1] = {NAME = 'split', DESC = 'Splits your character in half'}
  CMDs[#CMDs + 1] = {NAME = 'nilchar', DESC = 'Sets your characters parent to nil'}
  CMDs[#CMDs + 1] = {NAME = 'unnilchar / nonilchar', DESC = 'Sets your characters parent to workspace'}
  CMDs[#CMDs + 1] = {NAME = 'noroot / removeroot / rroot', DESC = 'Removes your characters HumanoidRootPart'}
  CMDs[#CMDs + 1] = {NAME = 'replaceroot', DESC = 'Replaces your characters HumanoidRootPart'}
  CMDs[#CMDs + 1] = {NAME = 'clearcharappearance / clearchar / clrchar', DESC = 'Removes all accessory, shirt, pants, charactermesh, and bodycolors'}
  CMDs[#CMDs + 1] = {NAME = '', DESC = ''}
  CMDs[#CMDs + 1] = {NAME = 'animation / anim [ID] [speed]', DESC = 'Makes your character perform an animation (must be by roblox to replicate)'}
  CMDs[#CMDs + 1] = {NAME = 'dance', DESC = 'Makes you  d a n c e'}
  CMDs[#CMDs + 1] = {NAME = 'undance', DESC = 'Stops dance animations'}
  CMDs[#CMDs + 1] = {NAME = 'spasm', DESC = 'Makes you  c r a z y'}
  CMDs[#CMDs + 1] = {NAME = 'unspasm', DESC = 'Stops spasm'}
  CMDs[#CMDs + 1] = {NAME = 'headthrow', DESC = 'Simply makes you throw your head'}
  CMDs[#CMDs + 1] = {NAME = 'noanim', DESC = 'Disables your animations'}
  CMDs[#CMDs + 1] = {NAME = 'reanim', DESC = 'Restores your animations'}
  CMDs[#CMDs + 1] = {NAME = 'animspeed [num]', DESC = 'Changes the speed of your current animation'}
  CMDs[#CMDs + 1] = {NAME = 'copyanimation / copyanim / copyemote [player]', DESC = 'Copies someone elses animation'}
  CMDs[#CMDs + 1] = {NAME = 'loopanimation / loopanim', DESC = 'Loops your current animation'}
  CMDs[#CMDs + 1] = {NAME = 'stopanimations / stopanims', DESC = 'Stops running animations'}
  CMDs[#CMDs + 1] = {NAME = 'refreshanimations / refreshanims', DESC = 'Refreshes animations'}
  CMDs[#CMDs + 1] = {NAME = 'allowcustomanim / allowcustomanimations', DESC = 'Lets you use custom animation packs instead'}
  CMDs[#CMDs + 1] = {NAME = 'unallowcustomanim / unallowcustomanimations', DESC = 'Doesn\'t let you use custom animation packs instead'}
  CMDs[#CMDs + 1] = {NAME = '', DESC = ''}
  CMDs[#CMDs + 1] = {NAME = 'autoclick [click delay] [release delay]', DESC = 'Automatically clicks your mouse with a set delay'}
  CMDs[#CMDs + 1] = {NAME = 'unautoclick / noautoclick', DESC = 'Turns off autoclick'}
  CMDs[#CMDs + 1] = {NAME = 'autokeypress [key] [down delay] [up delay]', DESC = 'Automatically presses a key with a set delay'}
  CMDs[#CMDs + 1] = {NAME = 'unautokeypress', DESC = 'Stops autokeypress'}
  CMDs[#CMDs + 1] = {NAME = 'hovername', DESC = 'Shows a players username when your mouse is hovered over them'}
  CMDs[#CMDs + 1] = {NAME = 'unhovername / nohovername', DESC = 'Turns off hovername'}
  CMDs[#CMDs + 1] = {NAME = 'mousesensitivity / ms [0-10]', DESC = 'Sets your mouse sensitivity (affects first person and right click drag) (default is 1)'}
  CMDs[#CMDs + 1] = {NAME = 'clickdelete', DESC = 'Go to settings>Keybinds>Add for clicktp'}
  CMDs[#CMDs + 1] = {NAME = 'clickteleport', DESC = 'Go to settings>Keybinds>Add for click tp'}
  CMDs[#CMDs + 1] = {NAME = '', DESC = ''}
  CMDs[#CMDs + 1] = {NAME = 'tools', DESC = 'Copies tools from ReplicatedStorage and Lighting'}
  CMDs[#CMDs + 1] = {NAME = 'notools / removetools / deletetools', DESC = 'Removes tools from character and backpack'}
  CMDs[#CMDs + 1] = {NAME = 'deleteselectedtool / dst', DESC = 'Removes any currently selected tools'}
  CMDs[#CMDs + 1] = {NAME = 'grabtools', DESC = 'Automatically get tools that are dropped'}
  CMDs[#CMDs + 1] = {NAME = 'ungrabtools / nograbtools', DESC = 'Disables grabtools'}
  CMDs[#CMDs + 1] = {NAME = 'copytools [player] (CLIENT)', DESC = 'Copies a players tools'}
  CMDs[#CMDs + 1] = {NAME = 'dupetools / clonetools [num]', DESC = 'Duplicates your inventory tools a set ammount of times'}
  CMDs[#CMDs + 1] = {NAME = 'givetool / givetools', DESC = 'Gives all the tools you\'re holding to [player] using the attach method.'}
  CMDs[#CMDs + 1] = {NAME = 'droptools', DESC = 'Drops your tools'}
  CMDs[#CMDs + 1] = {NAME = 'droppabletools', DESC = 'Makes your tools droppable'}
  CMDs[#CMDs + 1] = {NAME = 'equiptools', DESC = 'Equips every tool in your inventory at once'}
  CMDs[#CMDs + 1] = {NAME = 'unequiptools', DESC = 'Unequips every tool you are currently holding at once'}
  CMDs[#CMDs + 1] = {NAME = 'removespecifictool [name]', DESC = 'Automatically remove a specific tool from your inventory'}
  CMDs[#CMDs + 1] = {NAME = 'unremovespecifictool [name]', DESC = 'Stops removing a specific tool from your inventory'}
  CMDs[#CMDs + 1] = {NAME = 'clearremovespecifictool', DESC = 'Stop removing all specific tools from your inventory'}
  CMDs[#CMDs + 1] = {NAME = 'reach [num]', DESC = 'Increases the hitbox of your held tool'}
  CMDs[#CMDs + 1] = {NAME = 'unreach / noreach', DESC = 'Turns off reach'}
  CMDs[#CMDs + 1] = {NAME = 'grippos [X Y Z]', DESC = 'Changes your current tools grip position'}
  CMDs[#CMDs + 1] = {NAME = 'usetools [ammount] [delay]', DESC = 'Activates all tools in your backpack at the same time'}
  CMDs[#CMDs + 1] = {NAME = '', DESC = ''}
  CMDs[#CMDs + 1] = {NAME = 'addalias [cmd] [alias]', DESC = 'Adds an alias to a command'}
  CMDs[#CMDs + 1] = {NAME = 'removealias [alias]', DESC = 'Removes a custom alias'}
  CMDs[#CMDs + 1] = {NAME = 'clraliases', DESC = 'Removes all custom aliases'}
  CMDs[#CMDs + 1] = {NAME = '', DESC = ''}
  CMDs[#CMDs + 1] = {NAME = 'addplugin / plugin [name]', DESC = 'Add a plugin via command'}
  CMDs[#CMDs + 1] = {NAME = 'removeplugin / deleteplugin [name]', DESC = 'Remove a plugin via command'}
  CMDs[#CMDs + 1] = {NAME = 'reloadplugin [name]', DESC = 'Reloads a plugin'}
  CMDs[#CMDs + 1] = {NAME = '', DESC = ''}
  CMDs[#CMDs + 1] = {NAME = 'breakloops / break (cmd loops)', DESC = 'Stops any cmd loops (;100^1^cmd)'}
  CMDs[#CMDs + 1] = {NAME = 'removecmd / deletecmd', DESC = 'Removes a command until the admin is reloaded'}
  CMDs[#CMDs + 1] = {NAME = 'tpwalk / teleportwalk [num]', DESC = 'Teleports you to your move direction'}
  CMDs[#CMDs + 1] = {NAME = 'untpwalk / unteleportwalk', DESC = 'Undoes tpwalk / teleportwalk'}
  CMDs[#CMDs + 1] = {NAME = 'notifyping / ping', DESC = 'Notify yourself your ping'}
  CMDs[#CMDs + 1] = {NAME = 'trip', DESC = 'Makes your character fall over'}
  CMDs[#CMDs + 1] = {NAME = 'norender', DESC = 'Disable 3d Rendering to decrease the amount of CPU the client uses'}
  CMDs[#CMDs + 1] = {NAME = 'render', DESC = 'Enable 3d Rendering'}
  CMDs[#CMDs + 1] = {NAME = 'use2022materials / 2022materials', DESC = 'Enables 2022 material textures'}
  CMDs[#CMDs + 1] = {NAME = 'unuse2022materials / un2022materials', DESC = 'Disables 2022 material textures'}
  CMDs[#CMDs + 1] = {NAME = 'promptr6', DESC = 'Prompts the game to switch your rig type to R6'}
  CMDs[#CMDs + 1] = {NAME = 'promptr15', DESC = 'Prompts the game to switch your rig type to R15'}
  CMDs[#CMDs + 1] = {NAME = 'wallwalk / walkonwalls', DESC = 'Walk on walls'}
  wait()
  
  for i = 1, #CMDs do
    local newcmd = Example:Clone()
    newcmd.Parent = CMDsF
    newcmd.Visible = false
    newcmd.Text = CMDs[i].NAME
    newcmd.Name = "CMD"
    table.insert(text1, newcmd)
    if CMDs[i].DESC ~= "" then
      newcmd:SetAttribute("Title", CMDs[i].NAME)
      newcmd:SetAttribute("Desc", CMDs[i].DESC)
      newcmd.MouseButton1Down:Connect(function()
        if not IsOnMobile and newcmd.Visible and newcmd.TextTransparency == 0 then
          local currentText = Cmdbar.Text
          Cmdbar:CaptureFocus()
          autoComplete(newcmd.Text, currentText)
          maximizeHolder()
        end
      end)
    end
  end
  
  IndexContents("", true)
  
  function checkTT()
    local t
    local guisAtPosition = COREGUI:GetGuiObjectsAtPosition(IYMouse.X, IYMouse.Y)
  
    for _, gui in pairs(guisAtPosition) do
      if gui.Parent == CMDsF then
        t = gui
      end
    end
  
    if t ~= nil and t:GetAttribute("Title") ~= nil then
      local x = IYMouse.X
      local y = IYMouse.Y
      local xP
      local yP
      if IYMouse.X > 200 then
        xP = x - 201
      else
        xP = x + 21
      end
      if IYMouse.Y > (IYMouse.ViewSizeY-96) then
        yP = y - 97
      else
        yP = y
      end
      Tooltip.Position = UDim2.new(0, xP, 0, yP)
      Description.Text = t:GetAttribute("Desc")
      if t:GetAttribute("Title") ~= nil then
        Title_3.Text = t:GetAttribute("Title")
      else
        Title_3.Text = ''
      end
      Tooltip.Visible = true
    else
      Tooltip.Visible = false
    end
  end
  
  function FindInTable(tbl,val)
    if tbl == nil then return false end
    for _,v in pairs(tbl) do
      if v == val then return true end
    end 
    return false
  end
  
  function GetInTable(Table, Name)
    for i = 1, #Table do
      if Table[i] == Name then
        return i
      end
    end
    return false
  end
  
  function respawn(plr)
    if invisRunning then TurnVisible() end
    local char = plr.Character
    if char:FindFirstChildOfClass("Humanoid") then char:FindFirstChildOfClass("Humanoid"):ChangeState(15) end
    char:ClearAllChildren()
    local newChar = Instance.new("Model")
    newChar.Parent = workspace
    plr.Character = newChar
    wait()
    plr.Character = char
    newChar:Destroy()
  end
  
  local refreshCmd = false
  function refresh(plr)
    refreshCmd = true
    local Human = plr.Character and plr.Character:FindFirstChildOfClass("Humanoid", true)
    local pos = Human and Human.RootPart and Human.RootPart.CFrame
    local pos1 = workspace.CurrentCamera.CFrame
    respawn(plr)
    task.spawn(function()
      plr.CharacterAdded:Wait():WaitForChild("Humanoid").RootPart.CFrame, workspace.CurrentCamera.CFrame = pos, wait() and pos1
      refreshCmd = false
    end)
  end
  
  local lastDeath
  
  function onDied()
    task.spawn(function()
      if pcall(function() Players.LocalPlayer.Character:FindFirstChildOfClass('Humanoid') end) and Players.LocalPlayer.Character:FindFirstChildOfClass('Humanoid') then
        Players.LocalPlayer.Character:FindFirstChildOfClass('Humanoid').Died:Connect(function()
          if getRoot(Players.LocalPlayer.Character) then
            lastDeath = getRoot(Players.LocalPlayer.Character).CFrame
          end
        end)
      else
        wait(2)
        onDied()
      end
    end)
  end
  
  Clip = true
  spDelay = 0.1
  Players.LocalPlayer.CharacterAdded:Connect(function()
    NOFLY()
    Floating = false
  
    if not Clip then
      execCmd('clip')
    end
  
    repeat wait() until getRoot(Players.LocalPlayer.Character)
  
    pcall(function()
      if spawnpoint and not refreshCmd and spawnpos ~= nil then
        wait(spDelay)
        getRoot(Players.LocalPlayer.Character).CFrame = spawnpos
      end
    end)
  
    onDied()
  end)
  
  onDied()
  
  function getstring(begin)
    local start = begin-1
    local AA = '' for i,v in pairs(cargs) do
      if i > start then
        if AA ~= '' then
          AA = AA .. ' ' .. v
        else
          AA = AA .. v
        end
      end
    end
    return AA
  end
  
  findCmd=function(cmd_name)
    for i,v in pairs(cmds)do
      if v.NAME:lower()==cmd_name:lower() or FindInTable(v.ALIAS,cmd_name:lower()) then
        return v
      end
    end
    return customAlias[cmd_name:lower()]
  end
  
  function splitString(str,delim)
    local broken = {}
    if delim == nil then delim = "," end
    for w in string.gmatch(str,"[^"..delim.."]+") do
      table.insert(broken,w)
    end
    return broken
  end
  
  cmdHistory = {}
  local lastCmds = {}
  local historyCount = 0
  local split=" "
  local lastBreakTime = 0
  function execCmd(cmdStr,speaker,store)
    cmdStr = cmdStr:gsub("%s+$","")
    task.spawn(function()
      local rawCmdStr = cmdStr
      cmdStr = string.gsub(cmdStr,"\\\\","%%BackSlash%%")
      local commandsToRun = splitString(cmdStr,"\\")
      for i,v in pairs(commandsToRun) do
        v = string.gsub(v,"%%BackSlash%%","\\")
        local x,y,num = v:find("^(%d+)%^")
        local cmdDelay = 0
        local infTimes = false
        if num then
          v = v:sub(y+1)
          local x,y,del = v:find("^([%d%.]+)%^")
          if del then
            v = v:sub(y+1)
            cmdDelay = tonumber(del) or 0
          end
        else
          local x,y = v:find("^inf%^")
          if x then
            infTimes = true
            v = v:sub(y+1)
            local x,y,del = v:find("^([%d%.]+)%^")
            if del then
              v = v:sub(y+1)
              del = tonumber(del) or 1
              cmdDelay = (del > 0 and del or 1)
            else
              cmdDelay = 1
            end
          end
        end
        num = tonumber(num or 1)
  
        if v:sub(1,1) == "!" then
          local chunks = splitString(v:sub(2),split)
          if chunks[1] and lastCmds[chunks[1]] then v = lastCmds[chunks[1]] end
        end
  
        local args = splitString(v,split)
        local cmdName = args[1]
        local cmd = findCmd(cmdName)
        if cmd then
          table.remove(args,1)
          cargs = args
          if not speaker then speaker = Players.LocalPlayer end
          if store then
            if speaker == Players.LocalPlayer then
              if cmdHistory[1] ~= rawCmdStr and rawCmdStr:sub(1,11) ~= 'lastcommand' and rawCmdStr:sub(1,7) ~= 'lastcmd' then
                table.insert(cmdHistory,1,rawCmdStr)
              end
            end
            if #cmdHistory > 30 then table.remove(cmdHistory) end
  
            lastCmds[cmdName] = v
          end
          local cmdStartTime = tick()
          if infTimes then
            while lastBreakTime < cmdStartTime do
              local success,err = pcall(cmd.FUNC,args, speaker)
              if not success and _G.IY_DEBUG then
                warn("Command Error:", cmdName, err)
              end
              wait(cmdDelay)
            end
          else
            for rep = 1,num do
              if lastBreakTime > cmdStartTime then break end
              local success,err = pcall(function()
                cmd.FUNC(args, speaker)
              end)
              if not success and _G.IY_DEBUG then
                warn("Command Error:", cmdName, err)
              end
              if cmdDelay ~= 0 then wait(cmdDelay) end
            end
          end
        end
      end
    end)
  end	
  
  function addcmd(name,alias,func,plgn)
    cmds[#cmds+1]=
      {
        NAME=name;
        ALIAS=alias or {};
        FUNC=func;
        PLUGIN=plgn;
      }
  end
  
  function removecmd(cmd)
    if cmd ~= " " then
      for i = #cmds,1,-1 do
        if cmds[i].NAME == cmd or FindInTable(cmds[i].ALIAS,cmd) then
          table.remove(cmds, i)
          for a,c in pairs(CMDsF:GetChildren()) do
            if string.find(c.Text, "^"..cmd.."$") or string.find(c.Text, "^"..cmd.." ") or string.find(c.Text, " "..cmd.."$") or string.find(c.Text, " "..cmd.." ") then
              c.TextTransparency = 0.7
              c.MouseButton1Click:Connect(function()
                notify(c.Text, "Command has been disabled by you or a plugin")
              end)
            end
          end
        end
      end
    end
  end
  
  function addbind(cmd,key,iskeyup,toggle)
    if toggle then
      binds[#binds+1]=
        {
          COMMAND=cmd;
          KEY=key;
          ISKEYUP=iskeyup;
          TOGGLE = toggle;
        }
    else
      binds[#binds+1]=
        {
          COMMAND=cmd;
          KEY=key;
          ISKEYUP=iskeyup;
        }
    end
  end
  
  function addcmdtext(text,name,desc)
    local newcmd = Example:Clone()
    local tooltipText = tostring(text)
    local tooltipDesc = tostring(desc)
    newcmd.Parent = CMDsF
    newcmd.Visible = false
    newcmd.Text = text
    newcmd.Name = 'PLUGIN_'..name
    table.insert(text1,newcmd)
    if desc and desc ~= '' then
      newcmd:SetAttribute("Title", tooltipText)
      newcmd:SetAttribute("Desc", tooltipDesc)
      newcmd.MouseButton1Down:Connect(function()
        if newcmd.Visible and newcmd.TextTransparency == 0 then
          Cmdbar:CaptureFocus()
          autoComplete(newcmd.Text)
          maximizeHolder()
        end
      end)
    end
  end
  
  local WorldToScreen = function(Object)
    local ObjectVector = workspace.CurrentCamera:WorldToScreenPoint(Object.Position)
    return Vector2.new(ObjectVector.X, ObjectVector.Y)
  end
  
  local MousePositionToVector2 = function()
    return Vector2.new(IYMouse.X, IYMouse.Y)
  end
  
  local GetClosestPlayerFromCursor = function()
    local found = nil
    local ClosestDistance = math.huge
    for i, v in pairs(Players:GetPlayers()) do
      if v ~= Players.LocalPlayer and v.Character and v.Character:FindFirstChildOfClass("Humanoid") then
        for k, x in pairs(v.Character:GetChildren()) do
          if string.find(x.Name, "Torso") then
            local Distance = (WorldToScreen(x) - MousePositionToVector2()).Magnitude
            if Distance < ClosestDistance then
              ClosestDistance = Distance
              found = v
            end
          end
        end
      end
    end
    return found
  end
  
  SpecialPlayerCases = {
    ["all"] = function(speaker) return Players:GetPlayers() end,
    ["others"] = function(speaker)
      local plrs = {}
      for i,v in pairs(Players:GetPlayers()) do
        if v ~= speaker then
          table.insert(plrs,v)
        end
      end
      return plrs
    end,
    ["me"] = function(speaker)return {speaker} end,
    ["#(%d+)"] = function(speaker,args,currentList)
      local returns = {}
      local randAmount = tonumber(args[1])
      local players = {unpack(currentList)}
      for i = 1,randAmount do
        if #players == 0 then break end
        local randIndex = math.random(1,#players)
        table.insert(returns,players[randIndex])
        table.remove(players,randIndex)
      end
      return returns
    end,
    ["random"] = function(speaker,args,currentList)
      local players = Players:GetPlayers()
      local localplayer = Players.LocalPlayer
      table.remove(players, table.find(players, localplayer))
      return {players[math.random(1,#players)]}
    end,
    ["%%(.+)"] = function(speaker,args)
      local returns = {}
      local team = args[1]
      for _,plr in pairs(Players:GetPlayers()) do
        if plr.Team and string.sub(string.lower(plr.Team.Name),1,#team) == string.lower(team) then
          table.insert(returns,plr)
        end
      end
      return returns
    end,
    ["allies"] = function(speaker)
      local returns = {}
      local team = speaker.Team
      for _,plr in pairs(Players:GetPlayers()) do
        if plr.Team == team then
          table.insert(returns,plr)
        end
      end
      return returns
    end,
    ["enemies"] = function(speaker)
      local returns = {}
      local team = speaker.Team
      for _,plr in pairs(Players:GetPlayers()) do
        if plr.Team ~= team then
          table.insert(returns,plr)
        end
      end
      return returns
    end,
    ["team"] = function(speaker)
      local returns = {}
      local team = speaker.Team
      for _,plr in pairs(Players:GetPlayers()) do
        if plr.Team == team then
          table.insert(returns,plr)
        end
      end
      return returns
    end,
    ["nonteam"] = function(speaker)
      local returns = {}
      local team = speaker.Team
      for _,plr in pairs(Players:GetPlayers()) do
        if plr.Team ~= team then
          table.insert(returns,plr)
        end
      end
      return returns
    end,
    ["friends"] = function(speaker,args)
      local returns = {}
      for _,plr in pairs(Players:GetPlayers()) do
        if plr:IsFriendsWith(speaker.UserId) and plr ~= speaker then
          table.insert(returns,plr)
        end
      end
      return returns
    end,
    ["nonfriends"] = function(speaker,args)
      local returns = {}
      for _,plr in pairs(Players:GetPlayers()) do
        if not plr:IsFriendsWith(speaker.UserId) and plr ~= speaker then
          table.insert(returns,plr)
        end
      end
      return returns
    end,
    ["guests"] = function(speaker,args)
      local returns = {}
      for _,plr in pairs(Players:GetPlayers()) do
        if plr.Guest then
          table.insert(returns,plr)
        end
      end
      return returns
    end,
    ["bacons"] = function(speaker,args)
      local returns = {}
      for _,plr in pairs(Players:GetPlayers()) do
        if plr.Character:FindFirstChild('Pal Hair') or plr.Character:FindFirstChild('Kate Hair') then
          table.insert(returns,plr)
        end
      end
      return returns
    end,
    ["age(%d+)"] = function(speaker,args)
      local returns = {}
      local age = tonumber(args[1])
      if not age == nil then return end
      for _,plr in pairs(Players:GetPlayers()) do
        if plr.AccountAge <= age then
          table.insert(returns,plr)
        end
      end
      return returns
    end,
    ["nearest"] = function(speaker,args,currentList)
      local speakerChar = speaker.Character
      if not speakerChar or not getRoot(speakerChar) then return end
      local lowest = math.huge
      local NearestPlayer = nil
      for _,plr in pairs(currentList) do
        if plr ~= speaker and plr.Character then
          local distance = plr:DistanceFromCharacter(getRoot(speakerChar).Position)
          if distance < lowest then
            lowest = distance
            NearestPlayer = {plr}
          end
        end
      end
      return NearestPlayer
    end,
    ["farthest"] = function(speaker,args,currentList)
      local speakerChar = speaker.Character
      if not speakerChar or not getRoot(speakerChar) then return end
      local highest = 0
      local Farthest = nil
      for _,plr in pairs(currentList) do
        if plr ~= speaker and plr.Character then
          local distance = plr:DistanceFromCharacter(getRoot(speakerChar).Position)
          if distance > highest then
            highest = distance
            Farthest = {plr}
          end
        end
      end
      return Farthest
    end,
    ["group(%d+)"] = function(speaker,args)
      local returns = {}
      local groupID = tonumber(args[1])
      for _,plr in pairs(Players:GetPlayers()) do
        if plr:IsInGroup(groupID) then  
          table.insert(returns,plr)
        end
      end
      return returns
    end,
    ["alive"] = function(speaker,args)
      local returns = {}
      for _,plr in pairs(Players:GetPlayers()) do
        if plr.Character and plr.Character:FindFirstChildOfClass("Humanoid") and plr.Character:FindFirstChildOfClass("Humanoid").Health > 0 then
          table.insert(returns,plr)
        end
      end
      return returns
    end,
    ["dead"] = function(speaker,args)
      local returns = {}
      for _,plr in pairs(Players:GetPlayers()) do
        if (not plr.Character or not plr.Character:FindFirstChildOfClass("Humanoid")) or plr.Character:FindFirstChildOfClass("Humanoid").Health <= 0 then
          table.insert(returns,plr)
        end
      end
      return returns
    end,
    ["rad(%d+)"] = function(speaker,args)
      local returns = {}
      local radius = tonumber(args[1])
      local speakerChar = speaker.Character
      if not speakerChar or not getRoot(speakerChar) then return end
      for _,plr in pairs(Players:GetPlayers()) do
        if plr.Character and getRoot(plr.Character) then
          local magnitude = (getRoot(plr.Character).Position-getRoot(speakerChar).Position).magnitude
          if magnitude <= radius then table.insert(returns,plr) end
        end
      end
      return returns
    end,
    ["cursor"] = function(speaker)
      local plrs = {}
      local v = GetClosestPlayerFromCursor()
      if v ~= nil then table.insert(plrs, v) end
      return plrs
    end,
    ["npcs"] = function(speaker,args)
      local returns = {}
      for _, v in pairs(workspace:GetDescendants()) do
        if v:IsA("Model") and getRoot(v) and v:FindFirstChildWhichIsA("Humanoid") and Players:GetPlayerFromCharacter(v) == nil then
          local clone = Instance.new("Player")
          clone.Name = v.Name .. " - " .. v:FindFirstChildWhichIsA("Humanoid").DisplayName
          clone.Character = v
          table.insert(returns, clone)
        end
      end
      return returns
    end,
  }
  
  function toTokens(str)
    local tokens = {}
    for op,name in string.gmatch(str,"([+-])([^+-]+)") do
      table.insert(tokens,{Operator = op,Name = name})
    end
    return tokens
  end
  
  function onlyIncludeInTable(tab,matches)
    local matchTable = {}
    local resultTable = {}
    for i,v in pairs(matches) do matchTable[v.Name] = true end
    for i,v in pairs(tab) do if matchTable[v.Name] then table.insert(resultTable,v) end end
    return resultTable
  end
  
  function removeTableMatches(tab,matches)
    local matchTable = {}
    local resultTable = {}
    for i,v in pairs(matches) do matchTable[v.Name] = true end
    for i,v in pairs(tab) do if not matchTable[v.Name] then table.insert(resultTable,v) end end
    return resultTable
  end
  
  function getPlayersByName(Name)
    local Name,Len,Found = string.lower(Name),#Name,{}
    for _,v in pairs(Players:GetPlayers()) do
      if Name:sub(0,1) == '@' then
        if string.sub(string.lower(v.Name),1,Len-1) == Name:sub(2) then
          table.insert(Found,v)
        end
      else
        if string.sub(string.lower(v.Name),1,Len) == Name or string.sub(string.lower(v.DisplayName),1,Len) == Name then
          table.insert(Found,v)
        end
      end
    end
    return Found
  end
  
  function getPlayer(list,speaker)
    if list == nil then return {speaker.Name} end
    local nameList = splitString(list,",")
  
    local foundList = {}
  
    for _,name in pairs(nameList) do
      if string.sub(name,1,1) ~= "+" and string.sub(name,1,1) ~= "-" then name = "+"..name end
      local tokens = toTokens(name)
      local initialPlayers = Players:GetPlayers()
  
      for i,v in pairs(tokens) do
        if v.Operator == "+" then
          local tokenContent = v.Name
          local foundCase = false
          for regex,case in pairs(SpecialPlayerCases) do
            local matches = {string.match(tokenContent,"^"..regex.."$")}
            if #matches > 0 then
              foundCase = true
              initialPlayers = onlyIncludeInTable(initialPlayers,case(speaker,matches,initialPlayers))
            end
          end
          if not foundCase then
            initialPlayers = onlyIncludeInTable(initialPlayers,getPlayersByName(tokenContent))
          end
        else
          local tokenContent = v.Name
          local foundCase = false
          for regex,case in pairs(SpecialPlayerCases) do
            local matches = {string.match(tokenContent,"^"..regex.."$")}
            if #matches > 0 then
              foundCase = true
              initialPlayers = removeTableMatches(initialPlayers,case(speaker,matches,initialPlayers))
            end
          end
          if not foundCase then
            initialPlayers = removeTableMatches(initialPlayers,getPlayersByName(tokenContent))
          end
        end
      end
  
      for i,v in pairs(initialPlayers) do table.insert(foundList,v) end
    end
  
    local foundNames = {}
    for i,v in pairs(foundList) do table.insert(foundNames,v.Name) end
  
    return foundNames
  end
  
  formatUsername = function(player)
      if player.DisplayName ~= player.Name then
          return string.format("%s (%s)", player.Name, player.DisplayName)
      end
      return player.Name
  end
  
  getprfx=function(strn)
    if strn:sub(1,string.len(prefix))==prefix then return{'cmd',string.len(prefix)+1}
    end return
  end
  
  function do_exec(str, plr)
    str = str:gsub('/e ', '')
    local t = getprfx(str)
    if not t then return end
    str = str:sub(t[2])
    if t[1]=='cmd' then
      execCmd(str, plr, true)
      IndexContents('',true,false,true)
      CMDsF.CanvasPosition = canvasPos
    end
  end
  
  lastTextBoxString,lastTextBoxCon,lastEnteredString = nil,nil,nil
  
  UserInputService.TextBoxFocused:Connect(function(obj)
    if lastTextBoxCon then lastTextBoxCon:Disconnect() end
    if obj == Cmdbar then lastTextBoxString = nil return end
    lastTextBoxString = obj.Text
    lastTextBoxCon = obj:GetPropertyChangedSignal("Text"):Connect(function()
      if not (UserInputService:IsKeyDown(Enum.KeyCode.Return) or UserInputService:IsKeyDown(Enum.KeyCode.KeypadEnter)) then
        lastTextBoxString = obj.Text
      end
    end)
  end)
  
  UserInputService.InputBegan:Connect(function(input,gameProcessed)
    if gameProcessed then
      if Cmdbar and Cmdbar:IsFocused() then
        if input.KeyCode == Enum.KeyCode.Up then
          historyCount = historyCount + 1
          if historyCount > #cmdHistory then historyCount = #cmdHistory end
          Cmdbar.Text = cmdHistory[historyCount] or ""
          Cmdbar.CursorPosition = 1020
        elseif input.KeyCode == Enum.KeyCode.Down then
          historyCount = historyCount - 1
          if historyCount < 0 then historyCount = 0 end
          Cmdbar.Text = cmdHistory[historyCount] or ""
          Cmdbar.CursorPosition = 1020
        end
      elseif input.KeyCode == Enum.KeyCode.Return or input.KeyCode == Enum.KeyCode.KeypadEnter then
        lastEnteredString = lastTextBoxString
      end
    end
  end)
  
  Players.LocalPlayer.Chatted:Connect(function()
    wait()
    if lastEnteredString then
      local message = lastEnteredString
      lastEnteredString = nil
      do_exec(message, Players.LocalPlayer)
    end
  end)
  
  Cmdbar.PlaceholderText = "Command Bar ("..prefix..")"
  Cmdbar:GetPropertyChangedSignal("Text"):Connect(function()
    if Cmdbar:IsFocused() then
      IndexContents(Cmdbar.Text,true,true)
    end
  end)
  
  local tabComplete = nil
  tabAllowed = true
  Cmdbar.FocusLost:Connect(function(enterpressed)
    if enterpressed then
      local cmdbarText = Cmdbar.Text:gsub("^"..prefix,"")
      execCmd(cmdbarText,Players.LocalPlayer,true)
    end
    if tabComplete then tabComplete:Disconnect() end
    wait()
    if not Cmdbar:IsFocused() then
      Cmdbar.Text = ""
      IndexContents('',true,false,true)
      if SettingsOpen == true then
        wait(0.2)
        Settings:TweenPosition(UDim2.new(0, 0, 0, 45), "InOut", "Quart", 0.2, true, nil)
        CMDsF.Visible = false
      end
    end
    CMDsF.CanvasPosition = canvasPos
  end)
  
  Cmdbar.Focused:Connect(function()
    historyCount = 0
    canvasPos = CMDsF.CanvasPosition
    if SettingsOpen == true then
      wait(0.2)
      CMDsF.Visible = true
      Settings:TweenPosition(UDim2.new(0, 0, 0, 220), "InOut", "Quart", 0.2, true, nil)
    end
    tabComplete = UserInputService.InputBegan:Connect(function(input,gameProcessed)
      if Cmdbar:IsFocused() then
        if tabAllowed == true and input.KeyCode == Enum.KeyCode.Tab and topCommand ~= nil then
          autoComplete(topCommand)
        end
      else
        tabComplete:Disconnect()
      end
    end)
  end)
  
  ESPenabled = false
  CHMSenabled = false
  
  function round(num, numDecimalPlaces)
    local mult = 10^(numDecimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
  end
  
  function ESP(plr)
    task.spawn(function()
      for i,v in pairs(COREGUI:GetChildren()) do
        if v.Name == plr.Name..'_ESP' then
          v:Destroy()
        end
      end
      wait()
      if plr.Character and plr.Name ~= Players.LocalPlayer.Name and not COREGUI:FindFirstChild(plr.Name..'_ESP') then
        local ESPholder = Instance.new("Folder")
        ESPholder.Name = plr.Name..'_ESP'
        ESPholder.Parent = COREGUI
        repeat wait(1) until plr.Character and getRoot(plr.Character) and plr.Character:FindFirstChildOfClass("Humanoid")
        for b,n in pairs (plr.Character:GetChildren()) do
          if (n:IsA("BasePart")) then
            local a = Instance.new("BoxHandleAdornment")
            a.Name = plr.Name
            a.Parent = ESPholder
            a.Adornee = n
            a.AlwaysOnTop = true
            a.ZIndex = 10
            a.Size = n.Size
            a.Transparency = espTransparency
            a.Color = plr.TeamColor
          end
        end
        if plr.Character and plr.Character:FindFirstChild('Head') then
          local BillboardGui = Instance.new("BillboardGui")
          local TextLabel = Instance.new("TextLabel")
          BillboardGui.Adornee = plr.Character.Head
          BillboardGui.Name = plr.Name
          BillboardGui.Parent = ESPholder
          BillboardGui.Size = UDim2.new(0, 100, 0, 150)
          BillboardGui.StudsOffset = Vector3.new(0, 1, 0)
          BillboardGui.AlwaysOnTop = true
          TextLabel.Parent = BillboardGui
          TextLabel.BackgroundTransparency = 1
          TextLabel.Position = UDim2.new(0, 0, 0, -50)
          TextLabel.Size = UDim2.new(0, 100, 0, 100)
          TextLabel.Font = Enum.Font.SourceSansSemibold
          TextLabel.TextSize = 20
          TextLabel.TextColor3 = Color3.new(1, 1, 1)
          TextLabel.TextStrokeTransparency = 0
          TextLabel.TextYAlignment = Enum.TextYAlignment.Bottom
          TextLabel.Text = 'Name: '..plr.Name
          TextLabel.ZIndex = 10
          local espLoopFunc
          local teamChange
          local addedFunc
          addedFunc = plr.CharacterAdded:Connect(function()
            if ESPenabled then
              espLoopFunc:Disconnect()
              teamChange:Disconnect()
              ESPholder:Destroy()
              repeat wait(1) until getRoot(plr.Character) and plr.Character:FindFirstChildOfClass("Humanoid")
              ESP(plr)
              addedFunc:Disconnect()
            else
              teamChange:Disconnect()
              addedFunc:Disconnect()
            end
          end)
          teamChange = plr:GetPropertyChangedSignal("TeamColor"):Connect(function()
            if ESPenabled then
              espLoopFunc:Disconnect()
              addedFunc:Disconnect()
              ESPholder:Destroy()
              repeat wait(1) until getRoot(plr.Character) and plr.Character:FindFirstChildOfClass("Humanoid")
              ESP(plr)
              teamChange:Disconnect()
            else
              teamChange:Disconnect()
            end
          end)
          local function espLoop()
            if COREGUI:FindFirstChild(plr.Name..'_ESP') then
              if plr.Character and getRoot(plr.Character) and plr.Character:FindFirstChildOfClass("Humanoid") and Players.LocalPlayer.Character and getRoot(Players.LocalPlayer.Character) and Players.LocalPlayer.Character:FindFirstChildOfClass("Humanoid") then
                local pos = math.floor((getRoot(Players.LocalPlayer.Character).Position - getRoot(plr.Character).Position).magnitude)
                TextLabel.Text = 'Name: '..plr.Name..' | Health: '..round(plr.Character:FindFirstChildOfClass('Humanoid').Health, 1)..' | Studs: '..pos
              end
            else
              teamChange:Disconnect()
              addedFunc:Disconnect()
              espLoopFunc:Disconnect()
            end
          end
          espLoopFunc = RunService.RenderStepped:Connect(espLoop)
        end
      end
    end)
  end
  
  function CHMS(plr)
    task.spawn(function()
      for i,v in pairs(COREGUI:GetChildren()) do
        if v.Name == plr.Name..'_CHMS' then
          v:Destroy()
        end
      end
      wait()
      if plr.Character and plr.Name ~= Players.LocalPlayer.Name and not COREGUI:FindFirstChild(plr.Name..'_CHMS') then
        local ESPholder = Instance.new("Folder")
        ESPholder.Name = plr.Name..'_CHMS'
        ESPholder.Parent = COREGUI
        repeat wait(1) until plr.Character and getRoot(plr.Character) and plr.Character:FindFirstChildOfClass("Humanoid")
        for b,n in pairs (plr.Character:GetChildren()) do
          if (n:IsA("BasePart")) then
            local a = Instance.new("BoxHandleAdornment")
            a.Name = plr.Name
            a.Parent = ESPholder
            a.Adornee = n
            a.AlwaysOnTop = true
            a.ZIndex = 10
            a.Size = n.Size
            a.Transparency = espTransparency
            a.Color = plr.TeamColor
          end
        end
        local addedFunc
        local teamChange
        local CHMSremoved
        addedFunc = plr.CharacterAdded:Connect(function()
          if CHMSenabled then
            ESPholder:Destroy()
            teamChange:Disconnect()
            repeat wait(1) until getRoot(plr.Character) and plr.Character:FindFirstChildOfClass("Humanoid")
            CHMS(plr)
            addedFunc:Disconnect()
          else
            teamChange:Disconnect()
            addedFunc:Disconnect()
          end
        end)
        teamChange = plr:GetPropertyChangedSignal("TeamColor"):Connect(function()
          if CHMSenabled then
            ESPholder:Destroy()
            addedFunc:Disconnect()
            repeat wait(1) until getRoot(plr.Character) and plr.Character:FindFirstChildOfClass("Humanoid")
            CHMS(plr)
            teamChange:Disconnect()
          else
            teamChange:Disconnect()
          end
        end)
        CHMSremoved = ESPholder.AncestryChanged:Connect(function()
          teamChange:Disconnect()
          addedFunc:Disconnect()
          CHMSremoved:Disconnect()
        end)
      end
    end)
  end
  
  function Locate(plr)
    task.spawn(function()
      for i,v in pairs(COREGUI:GetChildren()) do
        if v.Name == plr.Name..'_LC' then
          v:Destroy()
        end
      end
      wait()
      if plr.Character and plr.Name ~= Players.LocalPlayer.Name and not COREGUI:FindFirstChild(plr.Name..'_LC') then
        local ESPholder = Instance.new("Folder")
        ESPholder.Name = plr.Name..'_LC'
        ESPholder.Parent = COREGUI
        repeat wait(1) until plr.Character and getRoot(plr.Character) and plr.Character:FindFirstChildOfClass("Humanoid")
        for b,n in pairs (plr.Character:GetChildren()) do
          if (n:IsA("BasePart")) then
            local a = Instance.new("BoxHandleAdornment")
            a.Name = plr.Name
            a.Parent = ESPholder
            a.Adornee = n
            a.AlwaysOnTop = true
            a.ZIndex = 10
            a.Size = n.Size
            a.Transparency = espTransparency
            a.Color = plr.TeamColor
          end
        end
        if plr.Character and plr.Character:FindFirstChild('Head') then
          local BillboardGui = Instance.new("BillboardGui")
          local TextLabel = Instance.new("TextLabel")
          BillboardGui.Adornee = plr.Character.Head
          BillboardGui.Name = plr.Name
          BillboardGui.Parent = ESPholder
          BillboardGui.Size = UDim2.new(0, 100, 0, 150)
          BillboardGui.StudsOffset = Vector3.new(0, 1, 0)
          BillboardGui.AlwaysOnTop = true
          TextLabel.Parent = BillboardGui
          TextLabel.BackgroundTransparency = 1
          TextLabel.Position = UDim2.new(0, 0, 0, -50)
          TextLabel.Size = UDim2.new(0, 100, 0, 100)
          TextLabel.Font = Enum.Font.SourceSansSemibold
          TextLabel.TextSize = 20
          TextLabel.TextColor3 = Color3.new(1, 1, 1)
          TextLabel.TextStrokeTransparency = 0
          TextLabel.TextYAlignment = Enum.TextYAlignment.Bottom
          TextLabel.Text = 'Name: '..plr.Name
          TextLabel.ZIndex = 10
          local lcLoopFunc
          local addedFunc
          local teamChange
          addedFunc = plr.CharacterAdded:Connect(function()
            if ESPholder ~= nil and ESPholder.Parent ~= nil then
              lcLoopFunc:Disconnect()
              teamChange:Disconnect()
              ESPholder:Destroy()
              repeat wait(1) until getRoot(plr.Character) and plr.Character:FindFirstChildOfClass("Humanoid")
              Locate(plr)
              addedFunc:Disconnect()
            else
              teamChange:Disconnect()
              addedFunc:Disconnect()
            end
          end)
          teamChange = plr:GetPropertyChangedSignal("TeamColor"):Connect(function()
            if ESPholder ~= nil and ESPholder.Parent ~= nil then
              lcLoopFunc:Disconnect()
              addedFunc:Disconnect()
              ESPholder:Destroy()
              repeat wait(1) until getRoot(plr.Character) and plr.Character:FindFirstChildOfClass("Humanoid")
              Locate(plr)
              teamChange:Disconnect()
            else
              teamChange:Disconnect()
            end
          end)
          local function lcLoop()
            if COREGUI:FindFirstChild(plr.Name..'_LC') then
              if plr.Character and getRoot(plr.Character) and plr.Character:FindFirstChildOfClass("Humanoid") and Players.LocalPlayer.Character and getRoot(Players.LocalPlayer.Character) and Players.LocalPlayer.Character:FindFirstChildOfClass("Humanoid") then
                local pos = math.floor((getRoot(Players.LocalPlayer.Character).Position - getRoot(plr.Character).Position).magnitude)
                TextLabel.Text = 'Name: '..plr.Name..' | Health: '..round(plr.Character:FindFirstChildOfClass('Humanoid').Health, 1)..' | Studs: '..pos
              end
            else
              teamChange:Disconnect()
              addedFunc:Disconnect()
              lcLoopFunc:Disconnect()
            end
          end
          lcLoopFunc = RunService.RenderStepped:Connect(lcLoop)
        end
      end
    end)
  end
  
  local bindsGUI = KeybindEditor
  local awaitingInput = false
  local keySelected = false
  
  function refreshbinds()
    if Holder_2 then
      Holder_2:ClearAllChildren()
      Holder_2.CanvasSize = UDim2.new(0, 0, 0, 10)
      for i = 1, #binds do
        local YSize = 25
        local Position = ((i * YSize) - YSize)
        local newbind = Example_2:Clone()
        newbind.Parent = Holder_2
        newbind.Visible = true
        newbind.Position = UDim2.new(0,0,0, Position + 5)
        table.insert(shade2,newbind)
        table.insert(shade2,newbind.Text)
        table.insert(text1,newbind.Text)
        table.insert(shade3,newbind.Text.Delete)
        table.insert(text2,newbind.Text.Delete)
        local input = tostring(binds[i].KEY)
        local key
        if input == 'RightClick' or input == 'LeftClick' then
          key = input
        else
          key = input:sub(14)
        end
        if binds[i].TOGGLE then
          newbind.Text.Text = key.." > "..binds[i].COMMAND.." / "..binds[i].TOGGLE
        else
          newbind.Text.Text = key.." > "..binds[i].COMMAND.."  "..(binds[i].ISKEYUP and "(keyup)" or "(keydown)")
        end
        Holder_2.CanvasSize = UDim2.new(0,0,0, Position + 30)
        newbind.Text.Delete.MouseButton1Click:Connect(function()
          unkeybind(binds[i].COMMAND,binds[i].KEY)
        end)
      end
    end
  end
  
  refreshbinds()
  
  toggleOn = {}
  
  function unkeybind(cmd,key)
    for i = #binds,1,-1 do
      if binds[i].COMMAND == cmd and binds[i].KEY == key then
        toggleOn[binds[i]] = nil
        table.remove(binds, i)
      end
    end
    refreshbinds()
    updatesaves()
    if key == 'RightClick' or key == 'LeftClick' then
      notify('Keybinds Updated','Unbinded '..key..' from '..cmd)
    else
      notify('Keybinds Updated','Unbinded '..key:sub(14)..' from '..cmd)
    end
  end
  
  PositionsFrame.Delete.MouseButton1Click:Connect(function()
    execCmd('cpos')
  end)
  
  function refreshwaypoints()
    if #WayPoints > 0 or #pWayPoints > 0 then
      PositionsHint:Destroy()
    end
    if Holder_4 then
      Holder_4:ClearAllChildren()
      Holder_4.CanvasSize = UDim2.new(0, 0, 0, 10)
      local YSize = 25
      local num = 1
      for i = 1, #WayPoints do
        local Position = ((num * YSize) - YSize)
        local newpoint = Example_4:Clone()
        newpoint.Parent = Holder_4
        newpoint.Visible = true
        newpoint.Position = UDim2.new(0,0,0, Position + 5)
        newpoint.Text.Text = WayPoints[i].NAME
        table.insert(shade2,newpoint)
        table.insert(shade2,newpoint.Text)
        table.insert(text1,newpoint.Text)
        table.insert(shade3,newpoint.Text.Delete)
        table.insert(text2,newpoint.Text.Delete)
        table.insert(shade3,newpoint.Text.TP)
        table.insert(text2,newpoint.Text.TP)
        Holder_4.CanvasSize = UDim2.new(0,0,0, Position + 30)
        newpoint.Text.Delete.MouseButton1Click:Connect(function()
          execCmd('dpos '..WayPoints[i].NAME)
        end)
        newpoint.Text.TP.MouseButton1Click:Connect(function()
          execCmd("loadpos "..WayPoints[i].NAME)
        end)
        num = num+1
      end
      for i = 1, #pWayPoints do
        local Position = ((num * YSize) - YSize)
        local newpoint = Example_4:Clone()
        newpoint.Parent = Holder_4
        newpoint.Visible = true
        newpoint.Position = UDim2.new(0,0,0, Position + 5)
        newpoint.Text.Text = pWayPoints[i].NAME
        table.insert(shade2,newpoint)
        table.insert(shade2,newpoint.Text)
        table.insert(text1,newpoint.Text)
        table.insert(shade3,newpoint.Text.Delete)
        table.insert(text2,newpoint.Text.Delete)
        table.insert(shade3,newpoint.Text.TP)
        table.insert(text2,newpoint.Text.TP)
        Holder_4.CanvasSize = UDim2.new(0,0,0, Position + 30)
        newpoint.Text.Delete.MouseButton1Click:Connect(function()
          execCmd('dpos '..pWayPoints[i].NAME)
        end)
        newpoint.Text.TP.MouseButton1Click:Connect(function()
          execCmd("loadpos "..pWayPoints[i].NAME)
        end)
        num = num+1
      end
    end
  end
  
  refreshwaypoints()
  
  function refreshaliases()
    if #aliases > 0 then
      AliasHint:Destroy()
    end
    if Holder_3 then
      Holder_3:ClearAllChildren()
      Holder_3.CanvasSize = UDim2.new(0, 0, 0, 10)
      for i = 1, #aliases do
        local YSize = 25
        local Position = ((i * YSize) - YSize)
        local newalias = Example_3:Clone()
        newalias.Parent = Holder_3
        newalias.Visible = true
        newalias.Position = UDim2.new(0,0,0, Position + 5)
        newalias.Text.Text = aliases[i].CMD.." > "..aliases[i].ALIAS
        table.insert(shade2,newalias)
        table.insert(shade2,newalias.Text)
        table.insert(text1,newalias.Text)
        table.insert(shade3,newalias.Text.Delete)
        table.insert(text2,newalias.Text.Delete)
        Holder_3.CanvasSize = UDim2.new(0,0,0, Position + 30)
        newalias.Text.Delete.MouseButton1Click:Connect(function()
          execCmd('removealias '..aliases[i].ALIAS)
        end)
      end
    end
  end
  
  local bindChosenKeyUp = false
  
  BindTo.MouseButton1Click:Connect(function()
    awaitingInput = true
    BindTo.Text = 'Press something'
  end)
  
  BindTriggerSelect.MouseButton1Click:Connect(function()
    bindChosenKeyUp = not bindChosenKeyUp
    BindTriggerSelect.Text = bindChosenKeyUp and "KeyUp" or "KeyDown"
  end)
  
  newToggle = false
  Cmdbar_3.Parent.Visible = false
  On_2.MouseButton1Click:Connect(function()
    if newToggle == false then newToggle = true
      On_2.BackgroundTransparency = 0
      Cmdbar_3.Parent.Visible = true
      BindTriggerSelect.Visible = false
    else newToggle = false
      On_2.BackgroundTransparency = 1
      Cmdbar_3.Parent.Visible = false
      BindTriggerSelect.Visible = true
    end
  end)
  
  Add_2.MouseButton1Click:Connect(function()
    if keySelected then
      if string.find(Cmdbar_2.Text, "\\\\") or string.find(Cmdbar_3.Text, "\\\\") then
        notify('Keybind Error','Only use one backslash to keybind multiple commands into one keybind or command')
      else
        if newToggle and Cmdbar_3.Text ~= '' and Cmdbar_2.text ~= '' then
          addbind(Cmdbar_2.Text,keyPressed,false,Cmdbar_3.Text)
        elseif not newToggle and Cmdbar_2.text ~= '' then
          addbind(Cmdbar_2.Text,keyPressed,bindChosenKeyUp)
        else
          return
        end
        refreshbinds()
        updatesaves()
        if keyPressed == 'RightClick' or keyPressed == 'LeftClick' then
          notify('Keybinds Updated','Binded '..keyPressed..' to '..Cmdbar_2.Text..(newToggle and " / "..Cmdbar_3.Text or ""))
        else
          notify('Keybinds Updated','Binded '..keyPressed:sub(14)..' to '..Cmdbar_2.Text..(newToggle and " / "..Cmdbar_3.Text or ""))
        end
      end
    end
  end)
  
  Exit_2.MouseButton1Click:Connect(function()
    Cmdbar_2.Text = 'Command'
    Cmdbar_3.Text = 'Command 2'
    BindTo.Text = 'Click to bind'
    bindChosenKeyUp = false
    BindTriggerSelect.Text = "KeyDown"
    keySelected = false
    KeybindEditor:TweenPosition(UDim2.new(0.5, -180, 0, -500), "InOut", "Quart", 0.5, true, nil)
  end)
  
  function onInputBegan(input,gameProcessed)
    if awaitingInput then
      if input.UserInputType == Enum.UserInputType.Keyboard then
        keyPressed = tostring(input.KeyCode)
        BindTo.Text = keyPressed:sub(14)
      elseif input.UserInputType == Enum.UserInputType.MouseButton1 then
        keyPressed = 'LeftClick'
        BindTo.Text = 'LeftClick'
      elseif input.UserInputType == Enum.UserInputType.MouseButton2 then
        keyPressed = 'RightClick'
        BindTo.Text = 'RightClick'
      end
      awaitingInput = false
      keySelected = true
    end
    if not gameProcessed and #binds > 0 then
      for i,v in pairs(binds) do
        if not v.ISKEYUP then
          if (input.UserInputType == Enum.UserInputType.Keyboard and v.KEY:lower()==tostring(input.KeyCode):lower()) or (input.UserInputType == Enum.UserInputType.MouseButton1 and v.KEY:lower()=='leftclick') or (input.UserInputType == Enum.UserInputType.MouseButton2 and v.KEY:lower()=='rightclick') then
            if v.TOGGLE then
              local isOn = toggleOn[v] == true
              toggleOn[v] = not isOn
              if isOn then
                execCmd(v.TOGGLE,Players.LocalPlayer)
              else
                execCmd(v.COMMAND,Players.LocalPlayer)
              end
            else
              execCmd(v.COMMAND,Players.LocalPlayer)
            end
          end
        end
      end
    end
  end
  
  function onInputEnded(input,gameProcessed)
    if not gameProcessed and #binds > 0 then
      for i,v in pairs(binds) do
        if v.ISKEYUP then
          if (input.UserInputType == Enum.UserInputType.Keyboard and v.KEY:lower()==tostring(input.KeyCode):lower()) or (input.UserInputType == Enum.UserInputType.MouseButton1 and v.KEY:lower()=='leftclick') or (input.UserInputType == Enum.UserInputType.MouseButton2 and v.KEY:lower()=='rightclick') then
            execCmd(v.COMMAND,Players.LocalPlayer)
          end
        end
      end
    end
  end
  
  UserInputService.InputBegan:Connect(onInputBegan)
  UserInputService.InputEnded:Connect(onInputEnded)
  
  ClickTP.Select.MouseButton1Click:Connect(function()
    if keySelected then
      addbind('clicktp',keyPressed,bindChosenKeyUp)
      refreshbinds()
      updatesaves()
      if keyPressed == 'RightClick' or keyPressed == 'LeftClick' then
        notify('Keybinds Updated','Binded '..keyPressed..' to click tp')
      else
        notify('Keybinds Updated','Binded '..keyPressed:sub(14)..' to click tp')
      end
    end
  end)
  
  ClickDelete.Select.MouseButton1Click:Connect(function()
    if keySelected then
      addbind('clickdel',keyPressed,bindChosenKeyUp)
      refreshbinds()
      updatesaves()
      if keyPressed == 'RightClick' or keyPressed == 'LeftClick' then
        notify('Keybinds Updated','Binded '..keyPressed..' to click delete')
      else
        notify('Keybinds Updated','Binded '..keyPressed:sub(14)..' to click delete')
      end
    end
  end)
  
  local function clicktpFunc()
    pcall(function()
      local character = Players.LocalPlayer.Character
      local humanoid = character:FindFirstChildOfClass("Humanoid")
      if humanoid and humanoid.SeatPart then
        humanoid.Sit = false
        wait(0.1)
      end
  
      local hipHeight = humanoid and humanoid.HipHeight > 0 and (humanoid.HipHeight + 1)
      local rootPart = getRoot(character)
      local rootPartPosition = rootPart.Position
      local hitPosition = IYMouse.Hit.Position
      local newCFrame = CFrame.new(
        hitPosition, 
        Vector3.new(rootPartPosition.X, hitPosition.Y, rootPartPosition.Z)
      ) * CFrame.Angles(0, math.pi, 0)
  
      rootPart.CFrame = newCFrame + Vector3.new(0, hipHeight or 4, 0)
    end)
  end
  
  IYMouse.Button1Down:Connect(function()
    for i,v in pairs(binds) do
      if v.COMMAND == 'clicktp' then
        local input = v.KEY
        if input == 'RightClick' and UserInputService:IsMouseButtonPressed(Enum.UserInputType.MouseButton2) and Players.LocalPlayer.Character then
          clicktpFunc()
        elseif input == 'LeftClick' and UserInputService:IsMouseButtonPressed(Enum.UserInputType.MouseButton1) and Players.LocalPlayer.Character then
          clicktpFunc()
        elseif UserInputService:IsKeyDown(Enum.KeyCode[input:sub(14)]) and Players.LocalPlayer.Character then
          clicktpFunc()
        end
      elseif v.COMMAND == 'clickdel' then
        local input = v.KEY
        if input == 'RightClick' and UserInputService:IsMouseButtonPressed(Enum.UserInputType.MouseButton2) then
          pcall(function() IYMouse.Target:Destroy() end)
        elseif input == 'LeftClick' and UserInputService:IsMouseButtonPressed(Enum.UserInputType.MouseButton1) then
          pcall(function() IYMouse.Target:Destroy() end)
        elseif UserInputService:IsKeyDown(Enum.KeyCode[input:sub(14)]) then
          pcall(function() IYMouse.Target:Destroy() end)
        end
      end
    end
  end)
  
  PluginsGUI = PluginEditor.background
  
  function addPlugin(name)
    if name:lower() == 'plugin file name' or name:lower() == 'iy_fe.iy' or name == 'iy_fe' then
      notify('Plugin Error','Please enter a valid plugin')
    else
      local file
      local fileName
      if name:sub(-3) == '.iy' then
        pcall(function() file = readfile(name) end)
        fileName = name
      else
        pcall(function() file = readfile(name..'.iy') end)
        fileName = name..'.iy'
      end
      if file then
        if not FindInTable(PluginsTable, fileName) then
          table.insert(PluginsTable, fileName)
          LoadPlugin(fileName)
          refreshplugins()
          pcall(eventEditor.Refresh)
        else
          notify('Plugin Error','This plugin is already added')
        end
      else
        notify('Plugin Error','Cannot locate file "'..fileName..'". Is the file in the correct folder?')
      end
    end
  end
  
  function deletePlugin(name)
    local pName = name..'.iy'
    if name:sub(-3) == '.iy' then
      pName = name
    end
    for i = #cmds,1,-1 do
      if cmds[i].PLUGIN == pName then
        table.remove(cmds, i)
      end
    end
    for i,v in pairs(CMDsF:GetChildren()) do
      if v.Name == 'PLUGIN_'..pName then
        v:Destroy()
      end
    end
    for i,v in pairs(PluginsTable) do
      if v == pName then
        table.remove(PluginsTable, i)
        notify('Removed Plugin',pName..' was removed')
      end
    end
    IndexContents('',true)
    refreshplugins()
  end
  
  function refreshplugins(dontSave)
    if #PluginsTable > 0 then
      PluginsHint:Destroy()
    end
    if Holder_5 then
      Holder_5:ClearAllChildren()
      Holder_5.CanvasSize = UDim2.new(0, 0, 0, 10)
      for i,v in pairs(PluginsTable) do
        local pName = v
        local YSize = 25
        local Position = ((i * YSize) - YSize)
        local newplugin = Example_5:Clone()
        newplugin.Parent = Holder_5
        newplugin.Visible = true
        newplugin.Position = UDim2.new(0,0,0, Position + 5)
        newplugin.Text.Text = pName
        table.insert(shade2,newplugin)
        table.insert(shade2,newplugin.Text)
        table.insert(text1,newplugin.Text)
        table.insert(shade3,newplugin.Text.Delete)
        table.insert(text2,newplugin.Text.Delete)
        Holder_5.CanvasSize = UDim2.new(0,0,0, Position + 30)
        newplugin.Text.Delete.MouseButton1Click:Connect(function()
          deletePlugin(pName)
        end)
      end
      if not dontSave then
        updatesaves()
      end
    end
  end
  
  local PluginCache
  function LoadPlugin(val,startup)
    local plugin
  
    function CatchedPluginLoad()
      plugin = loadfile(val)()
    end
  
    function handlePluginError(plerror)
      notify('Plugin Error','An error occurred with the plugin, "'..val..'" and it could not be loaded')
      if FindInTable(PluginsTable,val) then
        for i,v in pairs(PluginsTable) do
          if v == val then
            table.remove(PluginsTable,i)
          end
        end
      end
      updatesaves()
  
      print("Original Error: "..tostring(plerror))
      print("Plugin Error, stack traceback: "..tostring(debug.traceback()))
  
      plugin = nil
  
      return false
    end
  
    xpcall(CatchedPluginLoad, handlePluginError)
  
    if plugin ~= nil then
      if not startup then
        notify('Loaded Plugin',"Name: "..plugin["PluginName"].."\n".."Description: "..plugin["PluginDescription"])
      end
      addcmdtext('',val)
      addcmdtext(string.upper('--'..plugin["PluginName"]),val,plugin["PluginDescription"])
      if plugin["Commands"] then
        for i,v in pairs(plugin["Commands"]) do 
          local cmdExt = ''
          local cmdName = i
          local function handleNames()
            cmdName = i
            if findCmd(cmdName..cmdExt) then
              if isNumber(cmdExt) then
                cmdExt = cmdExt+1
              else
                cmdExt = 1
              end
              handleNames()
            else
              cmdName = cmdName..cmdExt
            end
          end
          handleNames()
          addcmd(cmdName, v["Aliases"], v["Function"], val)
          if v["ListName"] then
            local newName = v.ListName
            local cmdNames = {i,unpack(v.Aliases)}
            for i,v in pairs(cmdNames) do
              newName = newName:gsub(v,v..cmdExt)
            end
            addcmdtext(newName,val,v["Description"])
          else
            addcmdtext(cmdName,val,v["Description"])
          end
        end
      end
      IndexContents('',true)
    elseif plugin == nil then
      plugin = nil
    end
  end
  
  function FindPlugins()
    if PluginsTable ~= nil and type(PluginsTable) == "table" then
      for i,v in pairs(PluginsTable) do
        LoadPlugin(v,true)
      end
      refreshplugins(true)
    end
  end
  
  AddPlugin.MouseButton1Click:Connect(function()
    addPlugin(PluginsGUI.FileName.Text)
  end)
  
  Exit_3.MouseButton1Click:Connect(function()
    PluginEditor:TweenPosition(UDim2.new(0.5, -180, 0, -500), "InOut", "Quart", 0.5, true, nil)
    FileName.Text = 'Plugin File Name'
  end)
  
  Add_3.MouseButton1Click:Connect(function()
    PluginEditor:TweenPosition(UDim2.new(0.5, -180, 0, 310), "InOut", "Quart", 0.5, true, nil)
  end)
  
  Plugins.MouseButton1Click:Connect(function()
    if writefileExploit() then
      PluginsFrame:TweenPosition(UDim2.new(0, 0, 0, 0), "InOut", "Quart", 0.5, true, nil)
      wait(0.5)
      SettingsHolder.Visible = false
    else
      notify('Incompatible Exploit','Your exploit is unable to use plugins (missing read/writefile)')
    end
  end)
  
  Close_4.MouseButton1Click:Connect(function()
    SettingsHolder.Visible = true
    PluginsFrame:TweenPosition(UDim2.new(0, 0, 0, 175), "InOut", "Quart", 0.5, true, nil)
  end)
  
  local TeleportCheck = false
  Players.LocalPlayer.OnTeleport:Connect(function(State)
    if KeepInfYield and (not TeleportCheck) and queueteleport then
      TeleportCheck = true
      queueteleport("loadstring(game:HttpGet('https://raw.githubusercontent.com/EdgeIY/infiniteyield/master/source'))()")
    end
  end)
  
  addcmd('addalias',{},function(args, speaker)
    if #args < 2 then return end
    local cmd = string.lower(args[1])
    local alias = string.lower(args[2])
    for i,v in pairs(cmds) do
      if v.NAME:lower()==cmd or FindInTable(v.ALIAS,cmd) then
        customAlias[alias] = v
        aliases[#aliases + 1] = {CMD = cmd, ALIAS = alias}
        notify('Aliases Modified',"Added "..alias.." as an alias to "..cmd)
        updatesaves()
        refreshaliases()
        break
      end
    end
  end)
  
  addcmd('removealias',{},function(args, speaker)
    if #args < 1 then return end
    local alias = string.lower(args[1])
    if customAlias[alias] then
      local cmd = customAlias[alias].NAME
      customAlias[alias] = nil
      for i = #aliases,1,-1 do
        if aliases[i].ALIAS == tostring(alias) then
          table.remove(aliases, i)
        end
      end
      notify('Aliases Modified',"Removed the alias "..alias.." from "..cmd)
      updatesaves()
      refreshaliases()
    end
  end)
  
  addcmd('clraliases',{},function(args, speaker)
    customAlias = {}
    aliases = {}
    notify('Aliases Modified','Removed all aliases')
    updatesaves()
    refreshaliases()
  end)
  
  addcmd('discord', {'support', 'help'}, function(args, speaker)
    if everyClipboard then
      toClipboard('https://discord.com/invite/dYHag43eeU')
      notify('Discord Invite', 'Copied to clipboard!\ndiscord.gg/dYHag43eeU')
    else
      notify('Discord Invite', 'discord.gg/dYHag43eeU')
    end
    if httprequest then
      httprequest({
        Url = 'http://127.0.0.1:6463/rpc?v=1',
        Method = 'POST',
        Headers = {
          ['Content-Type'] = 'application/json',
          Origin = 'https://discord.com'
        },
        Body = HttpService:JSONEncode({
          cmd = 'INVITE_BROWSER',
          nonce = HttpService:GenerateGUID(false),
          args = {code = 'dYHag43eeU'}
        })
      })
    end
  end)
  
  addcmd('keepiy', {}, function(args, speaker)
    if queueteleport then
      KeepInfYield = true
      updatesaves()
    else
      notify('Incompatible Exploit','Your exploit does not support this command (missing queue_on_teleport)')
    end
  end)
  
  addcmd('unkeepiy', {}, function(args, speaker)
    if queueteleport then
      KeepInfYield = false
      updatesaves()
    else
      notify('Incompatible Exploit','Your exploit does not support this command (missing queue_on_teleport)')
    end
  end)
  
  addcmd('togglekeepiy', {}, function(args, speaker)
    if queueteleport then
      KeepInfYield = not KeepInfYield
      updatesaves()
    else
      notify('Incompatible Exploit','Your exploit does not support this command (missing queue_on_teleport)')
    end
  end)
  
  local canOpenServerinfo = true
  addcmd('serverinfo',{'info','sinfo'},function(args, speaker)
    if not canOpenServerinfo then return end
    canOpenServerinfo = false
    task.spawn(function()
      local FRAME = Instance.new("Frame")
      local shadow = Instance.new("Frame")
      local PopupText = Instance.new("TextLabel")
      local Exit = Instance.new("TextButton")
      local ExitImage = Instance.new("ImageLabel")
      local background = Instance.new("Frame")
      local TextLabel = Instance.new("TextLabel")
      local TextLabel2 = Instance.new("TextLabel")
      local TextLabel3 = Instance.new("TextLabel")
      local Time = Instance.new("TextLabel")
      local appearance = Instance.new("TextLabel")
      local maxplayers = Instance.new("TextLabel")
      local name = Instance.new("TextLabel")
      local placeid = Instance.new("TextLabel")
      local playerid = Instance.new("TextLabel")
      local players = Instance.new("TextLabel")
      local CopyApp = Instance.new("TextButton")
      local CopyPlrID = Instance.new("TextButton")
      local CopyPlcID = Instance.new("TextButton")
      local CopyPlcName = Instance.new("TextButton")
  
      FRAME.Name = randomString()
      FRAME.Parent = PARENT
      FRAME.Active = true
      FRAME.BackgroundTransparency = 1
      FRAME.Position = UDim2.new(0.5, -130, 0, -500)
      FRAME.Size = UDim2.new(0, 250, 0, 20)
      FRAME.ZIndex = 10
      dragGUI(FRAME)
  
      shadow.Name = "shadow"
      shadow.Parent = FRAME
      shadow.BackgroundColor3 = currentShade2
      shadow.BorderSizePixel = 0
      shadow.Size = UDim2.new(0, 250, 0, 20)
      shadow.ZIndex = 10
      table.insert(shade2,shadow)
  
      PopupText.Name = "PopupText"
      PopupText.Parent = shadow
      PopupText.BackgroundTransparency = 1
      PopupText.Size = UDim2.new(1, 0, 0.95, 0)
      PopupText.ZIndex = 10
      PopupText.Font = Enum.Font.SourceSans
      PopupText.TextSize = 14
      PopupText.Text = "Server"
      PopupText.TextColor3 = currentText1
      PopupText.TextWrapped = true
      table.insert(text1,PopupText)
  
      Exit.Name = "Exit"
      Exit.Parent = shadow
      Exit.BackgroundTransparency = 1
      Exit.Position = UDim2.new(1, -20, 0, 0)
      Exit.Size = UDim2.new(0, 20, 0, 20)
      Exit.Text = ""
      Exit.ZIndex = 10
  
      ExitImage.Parent = Exit
      ExitImage.BackgroundColor3 = Color3.new(1, 1, 1)
      ExitImage.BackgroundTransparency = 1
      ExitImage.Position = UDim2.new(0, 5, 0, 5)
      ExitImage.Size = UDim2.new(0, 10, 0, 10)
      ExitImage.Image = "rbxassetid://5054663650"
      ExitImage.ZIndex = 10
  
      background.Name = "background"
      background.Parent = FRAME
      background.Active = true
      background.BackgroundColor3 = currentShade1
      background.BorderSizePixel = 0
      background.Position = UDim2.new(0, 0, 1, 0)
      background.Size = UDim2.new(0, 250, 0, 250)
      background.ZIndex = 10
      table.insert(shade1,background)
  
      TextLabel.Name = "Text Label"
      TextLabel.Parent = background
      TextLabel.BackgroundTransparency = 1
      TextLabel.BorderSizePixel = 0
      TextLabel.Position = UDim2.new(0, 5, 0, 80)
      TextLabel.Size = UDim2.new(0, 100, 0, 20)
      TextLabel.ZIndex = 10
      TextLabel.Font = Enum.Font.SourceSansLight
      TextLabel.TextSize = 20
      TextLabel.Text = "Run Time:"
      TextLabel.TextColor3 = currentText1
      TextLabel.TextXAlignment = Enum.TextXAlignment.Left
      table.insert(text1,TextLabel)
  
      TextLabel2.Name = "Text Label2"
      TextLabel2.Parent = background
      TextLabel2.BackgroundTransparency = 1
      TextLabel2.BorderSizePixel = 0
      TextLabel2.Position = UDim2.new(0, 5, 0, 130)
      TextLabel2.Size = UDim2.new(0, 100, 0, 20)
      TextLabel2.ZIndex = 10
      TextLabel2.Font = Enum.Font.SourceSansLight
      TextLabel2.TextSize = 20
      TextLabel2.Text = "Statistics:"
      TextLabel2.TextColor3 = currentText1
      TextLabel2.TextXAlignment = Enum.TextXAlignment.Left
      table.insert(text1,TextLabel2)
  
      TextLabel3.Name = "Text Label3"
      TextLabel3.Parent = background
      TextLabel3.BackgroundTransparency = 1
      TextLabel3.BorderSizePixel = 0
      TextLabel3.Position = UDim2.new(0, 5, 0, 10)
      TextLabel3.Size = UDim2.new(0, 100, 0, 20)
      TextLabel3.ZIndex = 10
      TextLabel3.Font = Enum.Font.SourceSansLight
      TextLabel3.TextSize = 20
      TextLabel3.Text = "Local Player:"
      TextLabel3.TextColor3 = currentText1
      TextLabel3.TextXAlignment = Enum.TextXAlignment.Left
      table.insert(text1,TextLabel3)
  
      Time.Name = "Time"
      Time.Parent = background
      Time.BackgroundTransparency = 1
      Time.BorderSizePixel = 0
      Time.Position = UDim2.new(0, 5, 0, 105)
      Time.Size = UDim2.new(0, 100, 0, 20)
      Time.ZIndex = 10
      Time.Font = Enum.Font.SourceSans
      Time.FontSize = Enum.FontSize.Size14
      Time.Text = "LOADING"
      Time.TextColor3 = currentText1
      Time.TextXAlignment = Enum.TextXAlignment.Left
      table.insert(text1,Time)
  
      appearance.Name = "appearance"
      appearance.Parent = background
      appearance.BackgroundTransparency = 1
      appearance.BorderSizePixel = 0
      appearance.Position = UDim2.new(0, 5, 0, 55)
      appearance.Size = UDim2.new(0, 100, 0, 20)
      appearance.ZIndex = 10
      appearance.Font = Enum.Font.SourceSans
      appearance.FontSize = Enum.FontSize.Size14
      appearance.Text = "Appearance: LOADING"
      appearance.TextColor3 = currentText1
      appearance.TextXAlignment = Enum.TextXAlignment.Left
      table.insert(text1,appearance)
  
      maxplayers.Name = "maxplayers"
      maxplayers.Parent = background
      maxplayers.BackgroundTransparency = 1
      maxplayers.BorderSizePixel = 0
      maxplayers.Position = UDim2.new(0, 5, 0, 175)
      maxplayers.Size = UDim2.new(0, 100, 0, 20)
      maxplayers.ZIndex = 10
      maxplayers.Font = Enum.Font.SourceSans
      maxplayers.FontSize = Enum.FontSize.Size14
      maxplayers.Text = "LOADING"
      maxplayers.TextColor3 = currentText1
      maxplayers.TextXAlignment = Enum.TextXAlignment.Left
      table.insert(text1,maxplayers)
  
      name.Name = "name"
      name.Parent = background
      name.BackgroundTransparency = 1
      name.BorderSizePixel = 0
      name.Position = UDim2.new(0, 5, 0, 215)
      name.Size = UDim2.new(0, 240, 0, 30)
      name.ZIndex = 10
      name.Font = Enum.Font.SourceSans
      name.FontSize = Enum.FontSize.Size14
      name.Text = "Place Name: LOADING"
      name.TextColor3 = currentText1
      name.TextWrapped = true
      name.TextXAlignment = Enum.TextXAlignment.Left
      name.TextYAlignment = Enum.TextYAlignment.Top
      table.insert(text1,name)
  
      placeid.Name = "placeid"
      placeid.Parent = background
      placeid.BackgroundTransparency = 1
      placeid.BorderSizePixel = 0
      placeid.Position = UDim2.new(0, 5, 0, 195)
      placeid.Size = UDim2.new(0, 100, 0, 20)
      placeid.ZIndex = 10
      placeid.Font = Enum.Font.SourceSans
      placeid.FontSize = Enum.FontSize.Size14
      placeid.Text = "Place ID: LOADING"
      placeid.TextColor3 = currentText1
      placeid.TextXAlignment = Enum.TextXAlignment.Left
      table.insert(text1,placeid)
  
      playerid.Name = "playerid"
      playerid.Parent = background
      playerid.BackgroundTransparency = 1
      playerid.BorderSizePixel = 0
      playerid.Position = UDim2.new(0, 5, 0, 35)
      playerid.Size = UDim2.new(0, 100, 0, 20)
      playerid.ZIndex = 10
      playerid.Font = Enum.Font.SourceSans
      playerid.FontSize = Enum.FontSize.Size14
      playerid.Text = "Player ID: LOADING"
      playerid.TextColor3 = currentText1
      playerid.TextXAlignment = Enum.TextXAlignment.Left
      table.insert(text1,playerid)
  
      players.Name = "players"
      players.Parent = background
      players.BackgroundTransparency = 1
      players.BorderSizePixel = 0
      players.Position = UDim2.new(0, 5, 0, 155)
      players.Size = UDim2.new(0, 100, 0, 20)
      players.ZIndex = 10
      players.Font = Enum.Font.SourceSans
      players.FontSize = Enum.FontSize.Size14
      players.Text = "LOADING"
      players.TextColor3 = currentText1
      players.TextXAlignment = Enum.TextXAlignment.Left
      table.insert(text1,players)
  
      CopyApp.Name = "CopyApp"
      CopyApp.Parent = background
      CopyApp.BackgroundColor3 = currentShade2
      CopyApp.BorderSizePixel = 0
      CopyApp.Position = UDim2.new(0, 210, 0, 55)
      CopyApp.Size = UDim2.new(0, 35, 0, 20)
      CopyApp.Font = Enum.Font.SourceSans
      CopyApp.TextSize = 14
      CopyApp.Text = "Copy"
      CopyApp.TextColor3 = currentText1
      CopyApp.ZIndex = 10
      table.insert(shade2,CopyApp)
      table.insert(text1,CopyApp)
  
      CopyPlrID.Name = "CopyPlrID"
      CopyPlrID.Parent = background
      CopyPlrID.BackgroundColor3 = currentShade2
      CopyPlrID.BorderSizePixel = 0
      CopyPlrID.Position = UDim2.new(0, 210, 0, 35)
      CopyPlrID.Size = UDim2.new(0, 35, 0, 20)
      CopyPlrID.Font = Enum.Font.SourceSans
      CopyPlrID.TextSize = 14
      CopyPlrID.Text = "Copy"
      CopyPlrID.TextColor3 = currentText1
      CopyPlrID.ZIndex = 10
      table.insert(shade2,CopyPlrID)
      table.insert(text1,CopyPlrID)
  
      CopyPlcID.Name = "CopyPlcID"
      CopyPlcID.Parent = background
      CopyPlcID.BackgroundColor3 = currentShade2
      CopyPlcID.BorderSizePixel = 0
      CopyPlcID.Position = UDim2.new(0, 210, 0, 195)
      CopyPlcID.Size = UDim2.new(0, 35, 0, 20)
      CopyPlcID.Font = Enum.Font.SourceSans
      CopyPlcID.TextSize = 14
      CopyPlcID.Text = "Copy"
      CopyPlcID.TextColor3 = currentText1
      CopyPlcID.ZIndex = 10
      table.insert(shade2,CopyPlcID)
      table.insert(text1,CopyPlcID)
  
      CopyPlcName.Name = "CopyPlcName"
      CopyPlcName.Parent = background
      CopyPlcName.BackgroundColor3 = currentShade2
      CopyPlcName.BorderSizePixel = 0
      CopyPlcName.Position = UDim2.new(0, 210, 0, 215)
      CopyPlcName.Size = UDim2.new(0, 35, 0, 20)
      CopyPlcName.Font = Enum.Font.SourceSans
      CopyPlcName.TextSize = 14
      CopyPlcName.Text = "Copy"
      CopyPlcName.TextColor3 = currentText1
      CopyPlcName.ZIndex = 10
      table.insert(shade2,CopyPlcName)
      table.insert(text1,CopyPlcName)
  
      local SINFOGUI = background
      FRAME:TweenPosition(UDim2.new(0.5, -130, 0, 100), "InOut", "Quart", 0.5, true, nil) 
      wait(0.5)
      Exit.MouseButton1Click:Connect(function()
        FRAME:TweenPosition(UDim2.new(0.5, -130, 0, -500), "InOut", "Quart", 0.5, true, nil) 
        wait(0.6)
        FRAME:Destroy()
        canOpenServerinfo = true
      end)
      local Asset = MarketplaceService:GetProductInfo(PlaceId)
      SINFOGUI.name.Text = "Place Name: " .. Asset.Name
      SINFOGUI.playerid.Text = "Player ID: " ..speaker.UserId
      SINFOGUI.maxplayers.Text = Players.MaxPlayers.. " Players Max"
      SINFOGUI.placeid.Text = "Place ID: " ..PlaceId
  
      CopyApp.MouseButton1Click:Connect(function()
        toClipboard(speaker.CharacterAppearanceId)
      end)
      CopyPlrID.MouseButton1Click:Connect(function()
        toClipboard(speaker.UserId)
      end)
      CopyPlcID.MouseButton1Click:Connect(function()
        toClipboard(PlaceId)
      end)
      CopyPlcName.MouseButton1Click:Connect(function()
        toClipboard(Asset.Name)
      end)
  
      repeat
        players = Players:GetPlayers()
        SINFOGUI.players.Text = #players.. " Player(s)"
        SINFOGUI.appearance.Text = "Appearance: " ..speaker.CharacterAppearanceId
        local seconds = math.floor(workspace.DistributedGameTime)
        local minutes = math.floor(workspace.DistributedGameTime / 60)
        local hours = math.floor(workspace.DistributedGameTime / 60 / 60)
        local seconds = seconds - (minutes * 60)
        local minutes = minutes - (hours * 60)
        if hours < 1 then if minutes < 1 then
            SINFOGUI.Time.Text = seconds .. " Second(s)" else
            SINFOGUI.Time.Text = minutes .. " Minute(s), " .. seconds .. " Second(s)"
          end
        else
          SINFOGUI.Time.Text = hours .. " Hour(s), " .. minutes .. " Minute(s), " .. seconds .. " Second(s)"
        end
        wait(1)
      until SINFOGUI.Parent == nil
    end)
  end)
  
  addcmd('jobid',{},function(args, speaker)
    local jobId = 'Roblox.GameLauncher.joinGameInstance('..PlaceId..', "'..JobId..'")'
    toClipboard(jobId)
  end)
  
  addcmd('notifyjobid',{},function(args, speaker)
    notify('JobId / PlaceId',JobId..' / '..PlaceId)
  end)
  
  addcmd('breakloops',{'break'},function(args, speaker)
    lastBreakTime = tick()
  end)
  
  addcmd('gametp',{'gameteleport'},function(args, speaker)
    TeleportService:Teleport(args[1])
  end)
  
  addcmd("rejoin", {"rj"}, function(args, speaker)
    if #Players:GetPlayers() <= 1 then
      Players.LocalPlayer:Kick("\nRejoining...")
      wait()
      TeleportService:Teleport(PlaceId, Players.LocalPlayer)
    else
      TeleportService:TeleportToPlaceInstance(PlaceId, JobId, Players.LocalPlayer)
    end
  end)
  
  addcmd("autorejoin", {"autorj"}, function(args, speaker)
    GuiService.ErrorMessageChanged:Connect(function()
      execCmd("rejoin")
    end)
    notify("Auto Rejoin", "Auto rejoin enabled")
  end)
  
  addcmd("serverhop", {"shop"}, function(args, speaker)
      -- thanks to NoobSploit for fixing
      if httprequest then
          local servers = {}
          local req = httprequest({Url = string.format("https://games.roblox.com/v1/games/%d/servers/Public?sortOrder=Desc&limit=100&excludeFullGames=true", PlaceId)})
          local body = HttpService:JSONDecode(req.Body)
  
          if body and body.data then
              for i, v in next, body.data do
                  if type(v) == "table" and tonumber(v.playing) and tonumber(v.maxPlayers) and v.playing < v.maxPlayers and v.id ~= JobId then
                      table.insert(servers, 1, v.id)
                  end
              end
          end
  
          if #servers > 0 then
              TeleportService:TeleportToPlaceInstance(PlaceId, servers[math.random(1, #servers)], Players.LocalPlayer)
          else
              return notify("Serverhop", "Couldn't find a server.")
          end
      else
          notify("Incompatible Exploit", "Your exploit does not support this command (missing request)")
      end
  end)
  
  addcmd('joinplayer',{'joinp'},function(args, speaker)
    local retries = 0
    function ToServer(User,Place)	
      if args[2] == nil then Place = PlaceId end
      if not pcall(function()
          local FoundUser, UserId = pcall(function()
            if tonumber(User) then
              return tonumber(User)
            end
  
            return Players:GetUserIdFromNameAsync(User)
          end)
          if not FoundUser then
            notify('Join Error','Username/UserID does not exist')
          else
            notify('Join Player','Loading servers. Hold on a second.')
            local URL2 = ("https://games.roblox.com/v1/games/"..Place.."/servers/Public?sortOrder=Asc&limit=100")
            local Http = HttpService:JSONDecode(game:HttpGet(URL2))
            local GUID
  
            function tablelength(T)
              local count = 0
              for _ in pairs(T) do count = count + 1 end
              return count
            end
  
            for i=1,tonumber(tablelength(Http.data)) do
              for j,k in pairs(Http.data[i].playerIds) do
                if k == UserId then
                  GUID = Http.data[i].id
                end
              end
            end
  
            if GUID ~= nil then
              notify('Join Player','Joining '..User)
              TeleportService:TeleportToPlaceInstance(Place,GUID,Players.LocalPlayer)
            else
              notify('Join Error','Unable to join user.')
            end
          end
        end)
      then
        if retries < 3 then
          retries = retries + 1
          print('ERROR retrying '..retries..'/3')
          notify('Join Error','Error while trying to join. Retrying '..retries..'/3.')
          ToServer(User,Place)
        else
          notify('Join Error','Error while trying to join.')
        end
      end
    end
    ToServer(args[1],args[2])
  end)
  
  addcmd("exit", {}, function(args, speaker)
      game:Shutdown()
  end)
  
  local Noclipping = nil
  addcmd('noclip',{},function(args, speaker)
    Clip = false
    wait(0.1)
    local function NoclipLoop()
      if Clip == false and speaker.Character ~= nil then
        for _, child in pairs(speaker.Character:GetDescendants()) do
          if child:IsA("BasePart") and child.CanCollide == true and child.Name ~= floatName then
            child.CanCollide = false
          end
        end
      end
    end
    Noclipping = RunService.Stepped:Connect(NoclipLoop)
  end)
  
  addcmd('clip',{'unnoclip'},function(args, speaker)
    if Noclipping then
      Noclipping:Disconnect()
    end
    Clip = true
  end)
  
  addcmd('togglenoclip',{},function(args, speaker)
    if Clip then
      execCmd('noclip')
    else
      execCmd('clip')
    end
  end)
  
  FLYING = false
  QEfly = true
  iyflyspeed = 1
  vehicleflyspeed = 1
  function sFLY(vfly)
    repeat wait() until Players.LocalPlayer and Players.LocalPlayer.Character and getRoot(Players.LocalPlayer.Character) and Players.LocalPlayer.Character:FindFirstChildOfClass("Humanoid")
    repeat wait() until IYMouse
    if flyKeyDown or flyKeyUp then flyKeyDown:Disconnect() flyKeyUp:Disconnect() end
  
    local T = getRoot(Players.LocalPlayer.Character)
    local CONTROL = {F = 0, B = 0, L = 0, R = 0, Q = 0, E = 0}
    local lCONTROL = {F = 0, B = 0, L = 0, R = 0, Q = 0, E = 0}
    local SPEED = 0
  
    local function FLY()
      FLYING = true
      local BG = Instance.new('BodyGyro')
      local BV = Instance.new('BodyVelocity')
      BG.P = 9e4
      BG.Parent = T
      BV.Parent = T
      BG.maxTorque = Vector3.new(9e9, 9e9, 9e9)
      BG.cframe = T.CFrame
      BV.velocity = Vector3.new(0, 0, 0)
      BV.maxForce = Vector3.new(9e9, 9e9, 9e9)
      task.spawn(function()
        repeat wait()
          if not vfly and Players.LocalPlayer.Character:FindFirstChildOfClass('Humanoid') then
            Players.LocalPlayer.Character:FindFirstChildOfClass('Humanoid').PlatformStand = true
          end
          if CONTROL.L + CONTROL.R ~= 0 or CONTROL.F + CONTROL.B ~= 0 or CONTROL.Q + CONTROL.E ~= 0 then
            SPEED = 50
          elseif not (CONTROL.L + CONTROL.R ~= 0 or CONTROL.F + CONTROL.B ~= 0 or CONTROL.Q + CONTROL.E ~= 0) and SPEED ~= 0 then
            SPEED = 0
          end
          if (CONTROL.L + CONTROL.R) ~= 0 or (CONTROL.F + CONTROL.B) ~= 0 or (CONTROL.Q + CONTROL.E) ~= 0 then
            BV.velocity = ((workspace.CurrentCamera.CoordinateFrame.lookVector * (CONTROL.F + CONTROL.B)) + ((workspace.CurrentCamera.CoordinateFrame * CFrame.new(CONTROL.L + CONTROL.R, (CONTROL.F + CONTROL.B + CONTROL.Q + CONTROL.E) * 0.2, 0).p) - workspace.CurrentCamera.CoordinateFrame.p)) * SPEED
            lCONTROL = {F = CONTROL.F, B = CONTROL.B, L = CONTROL.L, R = CONTROL.R}
          elseif (CONTROL.L + CONTROL.R) == 0 and (CONTROL.F + CONTROL.B) == 0 and (CONTROL.Q + CONTROL.E) == 0 and SPEED ~= 0 then
            BV.velocity = ((workspace.CurrentCamera.CoordinateFrame.lookVector * (lCONTROL.F + lCONTROL.B)) + ((workspace.CurrentCamera.CoordinateFrame * CFrame.new(lCONTROL.L + lCONTROL.R, (lCONTROL.F + lCONTROL.B + CONTROL.Q + CONTROL.E) * 0.2, 0).p) - workspace.CurrentCamera.CoordinateFrame.p)) * SPEED
          else
            BV.velocity = Vector3.new(0, 0, 0)
          end
          BG.cframe = workspace.CurrentCamera.CoordinateFrame
        until not FLYING
        CONTROL = {F = 0, B = 0, L = 0, R = 0, Q = 0, E = 0}
        lCONTROL = {F = 0, B = 0, L = 0, R = 0, Q = 0, E = 0}
        SPEED = 0
        BG:Destroy()
        BV:Destroy()
        if Players.LocalPlayer.Character:FindFirstChildOfClass('Humanoid') then
          Players.LocalPlayer.Character:FindFirstChildOfClass('Humanoid').PlatformStand = false
        end
      end)
    end
    flyKeyDown = IYMouse.KeyDown:Connect(function(KEY)
      if KEY:lower() == 'w' then
        CONTROL.F = (vfly and vehicleflyspeed or iyflyspeed)
      elseif KEY:lower() == 's' then
        CONTROL.B = - (vfly and vehicleflyspeed or iyflyspeed)
      elseif KEY:lower() == 'a' then
        CONTROL.L = - (vfly and vehicleflyspeed or iyflyspeed)
      elseif KEY:lower() == 'd' then 
        CONTROL.R = (vfly and vehicleflyspeed or iyflyspeed)
      elseif QEfly and KEY:lower() == 'e' then
        CONTROL.Q = (vfly and vehicleflyspeed or iyflyspeed)*2
      elseif QEfly and KEY:lower() == 'q' then
        CONTROL.E = -(vfly and vehicleflyspeed or iyflyspeed)*2
      end
      pcall(function() workspace.CurrentCamera.CameraType = Enum.CameraType.Track end)
    end)
    flyKeyUp = IYMouse.KeyUp:Connect(function(KEY)
      if KEY:lower() == 'w' then
        CONTROL.F = 0
      elseif KEY:lower() == 's' then
        CONTROL.B = 0
      elseif KEY:lower() == 'a' then
        CONTROL.L = 0
      elseif KEY:lower() == 'd' then
        CONTROL.R = 0
      elseif KEY:lower() == 'e' then
        CONTROL.Q = 0
      elseif KEY:lower() == 'q' then
        CONTROL.E = 0
      end
    end)
    FLY()
  end
  
  function NOFLY()
    FLYING = false
    if flyKeyDown or flyKeyUp then flyKeyDown:Disconnect() flyKeyUp:Disconnect() end
    if Players.LocalPlayer.Character:FindFirstChildOfClass('Humanoid') then
      Players.LocalPlayer.Character:FindFirstChildOfClass('Humanoid').PlatformStand = false
    end
    pcall(function() workspace.CurrentCamera.CameraType = Enum.CameraType.Custom end)
  end
  
  local velocityHandlerName = randomString()
  local gyroHandlerName = randomString()
  local mfly1
  local mfly2
  
  local unmobilefly = function(speaker)
    pcall(function()
      FLYING = false
      local root = getRoot(speaker.Character)
      root:FindFirstChild(velocityHandlerName):Destroy()
      root:FindFirstChild(gyroHandlerName):Destroy()
      speaker.Character:FindFirstChildWhichIsA("Humanoid").PlatformStand = false
      mfly1:Disconnect()
      mfly2:Disconnect()
    end)
  end
  
  local mobilefly = function(speaker, vfly)
    unmobilefly(speaker)
    FLYING = true
  
    local root = getRoot(speaker.Character)
    local camera = workspace.CurrentCamera
    local v3none = Vector3.new()
    local v3zero = Vector3.new(0, 0, 0)
    local v3inf = Vector3.new(9e9, 9e9, 9e9)
  
    local controlModule = require(speaker.PlayerScripts:WaitForChild("PlayerModule"):WaitForChild("ControlModule"))
    local bv = Instance.new("BodyVelocity")
    bv.Name = velocityHandlerName
    bv.Parent = root
    bv.MaxForce = v3zero
    bv.Velocity = v3zero
  
    local bg = Instance.new("BodyGyro")
    bg.Name = gyroHandlerName
    bg.Parent = root
    bg.MaxTorque = v3inf
    bg.P = 1000
    bg.D = 50
  
    mfly1 = speaker.CharacterAdded:Connect(function()
      local bv = Instance.new("BodyVelocity")
      bv.Name = velocityHandlerName
      bv.Parent = root
      bv.MaxForce = v3zero
      bv.Velocity = v3zero
  
      local bg = Instance.new("BodyGyro")
      bg.Name = gyroHandlerName
      bg.Parent = root
      bg.MaxTorque = v3inf
      bg.P = 1000
      bg.D = 50
    end)
  
    mfly2 = RunService.RenderStepped:Connect(function()
      root = getRoot(speaker.Character)
      camera = workspace.CurrentCamera
      if speaker.Character:FindFirstChildWhichIsA("Humanoid") and root and root:FindFirstChild(velocityHandlerName) and root:FindFirstChild(gyroHandlerName) then
        local humanoid = speaker.Character:FindFirstChildWhichIsA("Humanoid")
        local VelocityHandler = root:FindFirstChild(velocityHandlerName)
        local GyroHandler = root:FindFirstChild(gyroHandlerName)
  
        VelocityHandler.MaxForce = v3inf
        GyroHandler.MaxTorque = v3inf
        if not vfly then humanoid.PlatformStand = true end
        GyroHandler.CFrame = camera.CoordinateFrame
        VelocityHandler.Velocity = v3none
  
        local direction = controlModule:GetMoveVector()
        if direction.X > 0 then
          VelocityHandler.Velocity = VelocityHandler.Velocity + camera.CFrame.RightVector * (direction.X * ((vfly and vehicleflyspeed or iyflyspeed) * 50))
        end
        if direction.X < 0 then
          VelocityHandler.Velocity = VelocityHandler.Velocity + camera.CFrame.RightVector * (direction.X * ((vfly and vehicleflyspeed or iyflyspeed) * 50))
        end
        if direction.Z > 0 then
          VelocityHandler.Velocity = VelocityHandler.Velocity - camera.CFrame.LookVector * (direction.Z * ((vfly and vehicleflyspeed or iyflyspeed) * 50))
        end
        if direction.Z < 0 then
          VelocityHandler.Velocity = VelocityHandler.Velocity - camera.CFrame.LookVector * (direction.Z * ((vfly and vehicleflyspeed or iyflyspeed) * 50))
        end
      end
    end)
  end
  
  addcmd('fly',{},function(args, speaker)
    if not IsOnMobile then
      NOFLY()
      wait()
      sFLY()
    else
      mobilefly(speaker)
    end
    if args[1] and isNumber(args[1]) then
      iyflyspeed = args[1]
    end
  end)
  
  addcmd('flyspeed',{'flysp'},function(args, speaker)
    local speed = args[1] or 1
    if isNumber(speed) then
      iyflyspeed = speed
    end
  end)
  
  addcmd('unfly',{'nofly','novfly','unvehiclefly','novehiclefly','unvfly'},function(args, speaker)
    if not IsOnMobile then NOFLY() else unmobilefly(speaker) end
  end)
  
  addcmd('vfly',{'vehiclefly'},function(args, speaker)
    if not IsOnMobile then
      NOFLY()
      wait()
      sFLY(true)
    else
      mobilefly(speaker, true)
    end
    if args[1] and isNumber(args[1]) then
      vehicleflyspeed = args[1]
    end
  end)
  
  addcmd('togglevfly',{},function(args, speaker)
    if FLYING then
      if not IsOnMobile then NOFLY() else unmobilefly(speaker) end
    else
      if not IsOnMobile then sFLY(true) else mobilefly(speaker, true) end
    end
  end)
  
  addcmd('vflyspeed',{'vflysp','vehicleflyspeed','vehicleflysp'},function(args, speaker)
    local speed = args[1] or 1
    if isNumber(speed) then
      vehicleflyspeed = speed
    end
  end)
  
  addcmd('qefly',{'flyqe'},function(args, speaker)
    if args[1] == 'false' then
      QEfly = false
    else
      QEfly = true
    end
  end)
  
  addcmd('togglefly',{},function(args, speaker)
    if FLYING then
      if not IsOnMobile then NOFLY() else unmobilefly(speaker) end
    else
      if not IsOnMobile then sFLY() else mobilefly(speaker) end
    end
  end)
  
  CFspeed = 50
  addcmd('cframefly', {'cfly'}, function(args, speaker)
    -- Full credit to peyton#9148 (apeyton)
    speaker.Character:FindFirstChildOfClass('Humanoid').PlatformStand = true
    local Head = speaker.Character:WaitForChild("Head")
    Head.Anchored = true
    if CFloop then CFloop:Disconnect() end
    CFloop = RunService.Heartbeat:Connect(function(deltaTime)
      local moveDirection = speaker.Character:FindFirstChildOfClass('Humanoid').MoveDirection * (CFspeed * deltaTime)
      local headCFrame = Head.CFrame
      local cameraCFrame = workspace.CurrentCamera.CFrame
      local cameraOffset = headCFrame:ToObjectSpace(cameraCFrame).Position
      cameraCFrame = cameraCFrame * CFrame.new(-cameraOffset.X, -cameraOffset.Y, -cameraOffset.Z + 1)
      local cameraPosition = cameraCFrame.Position
      local headPosition = headCFrame.Position
  
      local objectSpaceVelocity = CFrame.new(cameraPosition, Vector3.new(headPosition.X, cameraPosition.Y, headPosition.Z)):VectorToObjectSpace(moveDirection)
      Head.CFrame = CFrame.new(headPosition) * (cameraCFrame - cameraPosition) * CFrame.new(objectSpaceVelocity)
    end)
  end)
  
  addcmd('uncframefly',{'uncfly'},function(args, speaker)
    if CFloop then
      CFloop:Disconnect()
      speaker.Character:FindFirstChildOfClass('Humanoid').PlatformStand = false
      local Head = speaker.Character:WaitForChild("Head")
      Head.Anchored = false
    end
  end)
  
  addcmd('cframeflyspeed',{'cflyspeed'},function(args, speaker)
    if isNumber(args[1]) then
      CFspeed = args[1]
    end
  end)
  
  Floating = false
  floatName = randomString()
  addcmd('float', {'platform'},function(args, speaker)
    Floating = true
    local pchar = speaker.Character
    if pchar and not pchar:FindFirstChild(floatName) then
      task.spawn(function()
        local Float = Instance.new('Part')
        Float.Name = floatName
        Float.Parent = pchar
        Float.Transparency = 1
        Float.Size = Vector3.new(2,0.2,1.5)
        Float.Anchored = true
        local FloatValue = -3.1
        Float.CFrame = getRoot(pchar).CFrame * CFrame.new(0,FloatValue,0)
        notify('Float','Float Enabled (Q = down & E = up)')
        qUp = IYMouse.KeyUp:Connect(function(KEY)
          if KEY == 'q' then
            FloatValue = FloatValue + 0.5
          end
        end)
        eUp = IYMouse.KeyUp:Connect(function(KEY)
          if KEY == 'e' then
            FloatValue = FloatValue - 0.5
          end
        end)
        qDown = IYMouse.KeyDown:Connect(function(KEY)
          if KEY == 'q' then
            FloatValue = FloatValue - 0.5
          end
        end)
        eDown = IYMouse.KeyDown:Connect(function(KEY)
          if KEY == 'e' then
            FloatValue = FloatValue + 0.5
          end
        end)
        floatDied = speaker.Character:FindFirstChildOfClass('Humanoid').Died:Connect(function()
          FloatingFunc:Disconnect()
          Float:Destroy()
          qUp:Disconnect()
          eUp:Disconnect()
          qDown:Disconnect()
          eDown:Disconnect()
          floatDied:Disconnect()
        end)
        local function FloatPadLoop()
          if pchar:FindFirstChild(floatName) and getRoot(pchar) then
            Float.CFrame = getRoot(pchar).CFrame * CFrame.new(0,FloatValue,0)
          else
            FloatingFunc:Disconnect()
            Float:Destroy()
            qUp:Disconnect()
            eUp:Disconnect()
            qDown:Disconnect()
            eDown:Disconnect()
            floatDied:Disconnect()
          end
        end			
        FloatingFunc = RunService.Heartbeat:Connect(FloatPadLoop)
      end)
    end
  end)
  
  addcmd('unfloat',{'nofloat','unplatform','noplatform'},function(args, speaker)
    Floating = false
    local pchar = speaker.Character
    notify('Float','Float Disabled')
    if pchar:FindFirstChild(floatName) then
      pchar:FindFirstChild(floatName):Destroy()
    end
    if floatDied then
      FloatingFunc:Disconnect()
      qUp:Disconnect()
      eUp:Disconnect()
      qDown:Disconnect()
      eDown:Disconnect()
      floatDied:Disconnect()
    end
  end)
  
  addcmd('togglefloat',{},function(args, speaker)
    if Floating then
      execCmd('unfloat')
    else
      execCmd('float')
    end
  end)
  
  swimming = false
  local oldgrav = workspace.Gravity
  local swimbeat = nil
  addcmd('swim',{},function(args, speaker)
    if not swimming and speaker and speaker.Character and speaker.Character:FindFirstChildWhichIsA("Humanoid") then
      oldgrav = workspace.Gravity
      workspace.Gravity = 0
      local swimDied = function()
        workspace.Gravity = oldgrav
        swimming = false
      end
      local Humanoid = speaker.Character:FindFirstChildWhichIsA("Humanoid")
      gravReset = Humanoid.Died:Connect(swimDied)
      local enums = Enum.HumanoidStateType:GetEnumItems()
      table.remove(enums, table.find(enums, Enum.HumanoidStateType.None))
      for i, v in pairs(enums) do
        Humanoid:SetStateEnabled(v, false)
      end
      Humanoid:ChangeState(Enum.HumanoidStateType.Swimming)
      swimbeat = RunService.Heartbeat:Connect(function()
        pcall(function()
          speaker.Character.HumanoidRootPart.Velocity = ((Humanoid.MoveDirection ~= Vector3.new() or UserInputService:IsKeyDown(Enum.KeyCode.Space)) and speaker.Character.HumanoidRootPart.Velocity or Vector3.new())
        end)
      end)
      swimming = true
    end
  end)
  
  addcmd('unswim',{'noswim'},function(args, speaker)
    if speaker and speaker.Character and speaker.Character:FindFirstChildWhichIsA("Humanoid") then
      workspace.Gravity = oldgrav
      swimming = false
      if gravReset then
        gravReset:Disconnect()
      end
      if swimbeat ~= nil then
        swimbeat:Disconnect()
        swimbeat = nil
      end
      local Humanoid = speaker.Character:FindFirstChildWhichIsA("Humanoid")
      local enums = Enum.HumanoidStateType:GetEnumItems()
      table.remove(enums, table.find(enums, Enum.HumanoidStateType.None))
      for i, v in pairs(enums) do
        Humanoid:SetStateEnabled(v, true)
      end
    end
  end)
  
  addcmd('toggleswim',{},function(args, speaker)
    if swimming then
      execCmd('unswim')
    else
      execCmd('swim')
    end
  end)
  
  addcmd('setwaypoint',{'swp','setwp','spos','saveposition','savepos'},function(args, speaker)
    local WPName = tostring(getstring(1))
    if getRoot(speaker.Character) then
      notify('Modified Waypoints',"Created waypoint: "..getstring(1))
      local torso = getRoot(speaker.Character)
      WayPoints[#WayPoints + 1] = {NAME = WPName, COORD = {math.floor(torso.Position.X), math.floor(torso.Position.Y), math.floor(torso.Position.Z)}, GAME = PlaceId}
      if AllWaypoints ~= nil then
        AllWaypoints[#AllWaypoints + 1] = {NAME = WPName, COORD = {math.floor(torso.Position.X), math.floor(torso.Position.Y), math.floor(torso.Position.Z)}, GAME = PlaceId}
      end
    end	
    refreshwaypoints()
    updatesaves()
  end)
  
  addcmd('waypointpos',{'wpp','setwaypointposition','setpos','setwaypoint','setwaypointpos'},function(args, speaker)
    local WPName = tostring(getstring(1))
    if getRoot(speaker.Character) then
      notify('Modified Waypoints',"Created waypoint: "..getstring(1))
      WayPoints[#WayPoints + 1] = {NAME = WPName, COORD = {args[2], args[3], args[4]}, GAME = PlaceId}
      if AllWaypoints ~= nil then
        AllWaypoints[#AllWaypoints + 1] = {NAME = WPName, COORD = {args[2], args[3], args[4]}, GAME = PlaceId}
      end
    end	
    refreshwaypoints()
    updatesaves()
  end)
  
  addcmd('waypoints',{'positions'},function(args, speaker)
    if SettingsOpen == false then SettingsOpen = true
      Settings:TweenPosition(UDim2.new(0, 0, 0, 45), "InOut", "Quart", 0.5, true, nil)
      CMDsF.Visible = false
    end
    KeybindsFrame:TweenPosition(UDim2.new(0, 0, 0, 175), "InOut", "Quart", 0.5, true, nil)
    AliasesFrame:TweenPosition(UDim2.new(0, 0, 0, 175), "InOut", "Quart", 0.5, true, nil)
    PluginsFrame:TweenPosition(UDim2.new(0, 0, 0, 175), "InOut", "Quart", 0.5, true, nil)
    PositionsFrame:TweenPosition(UDim2.new(0, 0, 0, 0), "InOut", "Quart", 0.5, true, nil)
    wait(0.5)
    SettingsHolder.Visible = false
    maximizeHolder()
  end)
  
  waypointParts = {}
  addcmd('showwaypoints',{'showwp','showwps'},function(args, speaker)
    execCmd('hidewaypoints')
    wait()
    for i,_ in pairs(WayPoints) do
      local x = WayPoints[i].COORD[1]
      local y = WayPoints[i].COORD[2]
      local z = WayPoints[i].COORD[3]
      local part = Instance.new("Part")
      part.Size = Vector3.new(5,5,5)
      part.CFrame = CFrame.new(x,y,z)
      part.Parent = workspace
      part.Anchored = true
      part.CanCollide = false
      table.insert(waypointParts,part)
      local view = Instance.new("BoxHandleAdornment")
      view.Adornee = part
      view.AlwaysOnTop = true
      view.ZIndex = 10
      view.Size = part.Size
      view.Parent = part
    end
    for i,v in pairs(pWayPoints) do
      local view = Instance.new("BoxHandleAdornment")
      view.Adornee = pWayPoints[i].COORD[1]
      view.AlwaysOnTop = true
      view.ZIndex = 10
      view.Size = pWayPoints[i].COORD[1].Size
      view.Parent = pWayPoints[i].COORD[1]
      table.insert(waypointParts,view)
    end
  end)
  
  addcmd('hidewaypoints',{'hidewp','hidewps'},function(args, speaker)
    for i,v in pairs(waypointParts) do
      v:Destroy()
    end
    waypointParts = {}
  end)
  
  addcmd('waypoint',{'wp','lpos','loadposition','loadpos'},function(args, speaker)
    local WPName = tostring(getstring(1))
    if speaker.Character then
      for i,_ in pairs(WayPoints) do
        if tostring(WayPoints[i].NAME):lower() == tostring(WPName):lower() then
          local x = WayPoints[i].COORD[1]
          local y = WayPoints[i].COORD[2]
          local z = WayPoints[i].COORD[3]
          getRoot(speaker.Character).CFrame = CFrame.new(x,y,z)
        end
      end
      for i,_ in pairs(pWayPoints) do
        if tostring(pWayPoints[i].NAME):lower() == tostring(WPName):lower() then
          getRoot(speaker.Character).CFrame = CFrame.new(pWayPoints[i].COORD[1].Position)
        end
      end
    end
  end)
  
  tweenSpeed = 1
  addcmd('tweenspeed',{'tspeed'},function(args, speaker)
    local newSpeed = args[1] or 1
    if tonumber(newSpeed) then
      tweenSpeed = tonumber(newSpeed)
    end
  end)
  
  addcmd('tweenwaypoint',{'twp'},function(args, speaker)
    local WPName = tostring(getstring(1))
    if speaker.Character then
      for i,_ in pairs(WayPoints) do
        local x = WayPoints[i].COORD[1]
        local y = WayPoints[i].COORD[2]
        local z = WayPoints[i].COORD[3]
        if tostring(WayPoints[i].NAME):lower() == tostring(WPName):lower() then
          TweenService:Create(getRoot(speaker.Character), TweenInfo.new(tweenSpeed, Enum.EasingStyle.Linear), {CFrame = CFrame.new(x,y,z)}):Play()
        end
      end
      for i,_ in pairs(pWayPoints) do
        if tostring(pWayPoints[i].NAME):lower() == tostring(WPName):lower() then
          TweenService:Create(getRoot(speaker.Character), TweenInfo.new(tweenSpeed, Enum.EasingStyle.Linear), {CFrame = CFrame.new(pWayPoints[i].COORD[1].Position)}):Play()
        end
      end
    end
  end)
  
  addcmd('walktowaypoint',{'wtwp'},function(args, speaker)
    local WPName = tostring(getstring(1))
    if speaker.Character then
      for i,_ in pairs(WayPoints) do
        local x = WayPoints[i].COORD[1]
        local y = WayPoints[i].COORD[2]
        local z = WayPoints[i].COORD[3]
        if tostring(WayPoints[i].NAME):lower() == tostring(WPName):lower() then
          if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
            speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
            wait(.1)
          end
          speaker.Character:FindFirstChildOfClass('Humanoid').WalkToPoint = Vector3.new(x,y,z)
        end
      end
      for i,_ in pairs(pWayPoints) do
        if tostring(pWayPoints[i].NAME):lower() == tostring(WPName):lower() then
          if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
            speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
            wait(.1)
          end
          speaker.Character:FindFirstChildOfClass('Humanoid').WalkToPoint = Vector3.new(pWayPoints[i].COORD[1].Position)
        end
      end
    end
  end)
  
  addcmd('deletewaypoint',{'dwp','dpos','deleteposition','deletepos'},function(args, speaker)
    for i,v in pairs(WayPoints) do
      if v.NAME:lower() == tostring(getstring(1)):lower() then
        notify('Modified Waypoints',"Deleted waypoint: " .. v.NAME)
        table.remove(WayPoints, i)
      end
    end
    if AllWaypoints ~= nil and #AllWaypoints > 0 then
      for i,v in pairs(AllWaypoints) do
        if v.NAME:lower() == tostring(getstring(1)):lower() then
          if not v.GAME or v.GAME == PlaceId then
            table.remove(AllWaypoints, i)
          end
        end
      end
    end
    for i,v in pairs(pWayPoints) do
      if v.NAME:lower() == tostring(getstring(1)):lower() then
        notify('Modified Waypoints',"Deleted waypoint: " .. v.NAME)
        table.remove(pWayPoints, i)
      end
    end
    refreshwaypoints()
    updatesaves()
  end)
  
  addcmd('clearwaypoints',{'cwp','clearpositions','cpos','clearpos'},function(args, speaker)
    WayPoints = {}
    pWayPoints = {}
    refreshwaypoints()
    updatesaves()
    AllWaypoints = {}
    notify('Modified Waypoints','Removed all waypoints')
  end)
  
  addcmd('cleargamewaypoints',{'cgamewp'},function(args, speaker)
    for i,v in pairs(WayPoints) do
      if v.GAME == PlaceId then
        table.remove(WayPoints, i)
      end
    end
    if AllWaypoints ~= nil and #AllWaypoints > 0 then
      for i,v in pairs(AllWaypoints) do
        if v.GAME == PlaceId then
          table.remove(AllWaypoints, i)
        end
      end
    end
    for i,v in pairs(pWayPoints) do
      if v.GAME == PlaceId then
        table.remove(pWayPoints, i)
      end
    end
    refreshwaypoints()
    updatesaves()
    notify('Modified Waypoints','Deleted game waypoints')
  end)
  
  
  local coreGuiTypeNames = {
    -- predefined aliases
    ["inventory"] = Enum.CoreGuiType.Backpack,
    ["leaderboard"] = Enum.CoreGuiType.PlayerList,
    ["emotes"] = Enum.CoreGuiType.EmotesMenu
  }
  
  -- Load the full list of enums
  for _, enumItem in ipairs(Enum.CoreGuiType:GetEnumItems()) do
    coreGuiTypeNames[enumItem.Name:lower()] = enumItem
  end
  
  addcmd('enable',{},function(args, speaker)
    local input = args[1] and args[1]:lower()
    if input then
      if input == "reset" then
        StarterGui:SetCore("ResetButtonCallback", true)
      else
        local coreGuiType = coreGuiTypeNames[input]
        if coreGuiType then
          StarterGui:SetCoreGuiEnabled(coreGuiType, true)
        end
      end
    end
  end)
  
  addcmd('disable',{},function(args, speaker)
    local input = args[1] and args[1]:lower()
    if input then
      if input == "reset" then
        StarterGui:SetCore("ResetButtonCallback", false)
      else
        local coreGuiType = coreGuiTypeNames[input]
        if coreGuiType then
          StarterGui:SetCoreGuiEnabled(coreGuiType, false)
        end
      end
    end
  end)
  
  
  local invisGUIS = {}
  addcmd('showguis',{},function(args, speaker)
    for i,v in pairs(speaker:FindFirstChildWhichIsA("PlayerGui"):GetDescendants()) do
      if (v:IsA("Frame") or v:IsA("ImageLabel") or v:IsA("ScrollingFrame")) and not v.Visible then
        v.Visible = true
        if not FindInTable(invisGUIS,v) then
          table.insert(invisGUIS,v)
        end
      end
    end
  end)
  
  addcmd('unshowguis',{},function(args, speaker)
    for i,v in pairs(invisGUIS) do
      v.Visible = false
    end
    invisGUIS = {}
  end)
  
  local hiddenGUIS = {}
  addcmd('hideguis',{},function(args, speaker)
    for i,v in pairs(speaker:FindFirstChildWhichIsA("PlayerGui"):GetDescendants()) do
      if (v:IsA("Frame") or v:IsA("ImageLabel") or v:IsA("ScrollingFrame")) and v.Visible then
        v.Visible = false
        if not FindInTable(hiddenGUIS,v) then
          table.insert(hiddenGUIS,v)
        end
      end
    end
  end)
  
  addcmd('unhideguis',{},function(args, speaker)
    for i,v in pairs(hiddenGUIS) do
      v.Visible = true
    end
    hiddenGUIS = {}
  end)
  
  function deleteGuisAtPos()
    pcall(function()
      local guisAtPosition = Players.LocalPlayer.PlayerGui:GetGuiObjectsAtPosition(IYMouse.X, IYMouse.Y)
      for _, gui in pairs(guisAtPosition) do
        if gui.Visible == true then
          gui:Destroy()
        end
      end
    end)
  end
  
  local deleteGuiInput
  addcmd('guidelete',{},function(args, speaker)
    deleteGuiInput = UserInputService.InputBegan:Connect(function(input, gameProcessedEvent)
      if not gameProcessedEvent then
        if input.KeyCode == Enum.KeyCode.Backspace then
          deleteGuisAtPos()
        end
      end
    end)
    notify('GUI Delete Enabled','Hover over a GUI and press backspace to delete it')
  end)
  
  addcmd('unguidelete',{'noguidelete'},function(args, speaker)
    if deleteGuiInput then deleteGuiInput:Disconnect() end
    notify('GUI Delete Disabled','GUI backspace delete has been disabled')
  end)
  
  local wasStayOpen = StayOpen
  addcmd('hideiy',{},function(args, speaker)
    isHidden = true
    wasStayOpen = StayOpen
    if StayOpen == true then
      StayOpen = false
      On.BackgroundTransparency = 1
    end
    minimizeNum = 0
    minimizeHolder()
    if not (args[1] and tostring(args[1]) == 'nonotify') then notify('IY Hidden','You can press the prefix key to access the command bar') end
  end)
  
  addcmd('showiy',{'unhideiy'},function(args, speaker)
    isHidden = false
    minimizeNum = -20
    if wasStayOpen then
      maximizeHolder()
      StayOpen = true
      On.BackgroundTransparency = 0
    else
      minimizeHolder()
    end
  end)
  
  addcmd('rec', {'record'}, function(args, speaker)
    return COREGUI:ToggleRecording()
  end)
  
  addcmd('screenshot', {'scrnshot'}, function(args, speaker)
    return COREGUI:TakeScreenshot()
  end)
  
  addcmd('togglefs', {'togglefullscreen'}, function(args, speaker)
    return GuiService:ToggleFullscreen()
  end)
  
  addcmd('inspect', {'examine'}, function(args, speaker)
    for _, v in ipairs(getPlayer(args[1], speaker)) do
      GuiService:CloseInspectMenu()
      GuiService:InspectPlayerFromUserId(Players[v].UserId)
    end
  end)
  
  addcmd("savegame", {"saveplace"}, function(args, speaker)
      if saveinstance then
          notify("Loading", "Downloading game. This will take a while")
          saveinstance()
          notify("Game Saved", "Saved place to the workspace folder within your exploit folder.")
      else
          notify("Incompatible Exploit", "Your exploit does not support this command (missing saveinstance)")
      end
  end)
  
  addcmd('clearerror',{'clearerrors'},function(args, speaker)
    GuiService:ClearError()
  end)
  
  addcmd('clientantikick',{'antikick'},function(args, speaker)
    if not hookmetamethod then 
      return notify('Incompatible Exploit','Your exploit does not support this command (missing hookmetamethod)')
    end
    local LocalPlayer = Players.LocalPlayer
    local oldhmmi
    local oldhmmnc
    oldhmmi = hookmetamethod(game, "__index", function(self, method)
      if self == LocalPlayer and method:lower() == "kick" then
        return error("Expected ':' not '.' calling member function Kick", 2)
      end
      return oldhmmi(self, method)
    end)
    oldhmmnc = hookmetamethod(game, "__namecall", function(self, ...)
      if self == LocalPlayer and getnamecallmethod():lower() == "kick" then
        return
      end
      return oldhmmnc(self, ...)
    end)
  
    notify('Client Antikick','Client anti kick is now active (only effective on localscript kick)')
  end)
  
  allow_rj = true
  addcmd('clientantiteleport',{'antiteleport'},function(args, speaker)
    if not hookmetamethod then 
      return notify('Incompatible Exploit','Your exploit does not support this command (missing hookmetamethod)')
    end
    local TeleportService = TeleportService
    local oldhmmi
    local oldhmmnc
    oldhmmi = hookmetamethod(game, "__index", function(self, method)
      if self == TeleportService then
        if method:lower() == "teleport" then
          return error("Expected ':' not '.' calling member function Kick", 2)
        elseif method == "TeleportToPlaceInstance" then
          return error("Expected ':' not '.' calling member function TeleportToPlaceInstance", 2)
        end
      end
      return oldhmmi(self, method)
    end)
    oldhmmnc = hookmetamethod(game, "__namecall", function(self, ...)
      if self == TeleportService and getnamecallmethod():lower() == "teleport" or getnamecallmethod() == "TeleportToPlaceInstance" then
        return
      end
      return oldhmmnc(self, ...)
    end)
  
    notify('Client AntiTP','Client anti teleport is now active (only effective on localscript teleport)')
  end)
  
  addcmd('allowrejoin',{'allowrj'},function(args, speaker)
    if args[1] and args[1] == 'false' then
      allow_rj = false
      notify('Client AntiTP','Allow rejoin set to false')
    else
      allow_rj = true
      notify('Client AntiTP','Allow rejoin set to true')
    end
  end)
  
  addcmd('cancelteleport',{'canceltp'},function(args, speaker)
    TeleportService:TeleportCancel()
  end)
  
  addcmd('volume',{'vol'},function(args, speaker)
    local level = args[1]/10
    UserSettings():GetService("UserGameSettings").MasterVolume = level
  end)
  
  addcmd('antilag',{'boostfps','lowgraphics'},function(args, speaker)
    local Terrain = workspace:FindFirstChildOfClass('Terrain')
    Terrain.WaterWaveSize = 0
    Terrain.WaterWaveSpeed = 0
    Terrain.WaterReflectance = 0
    Terrain.WaterTransparency = 0
    Lighting.GlobalShadows = false
    Lighting.FogEnd = 9e9
    settings().Rendering.QualityLevel = 1
    for i,v in pairs(game:GetDescendants()) do
      if v:IsA("Part") or v:IsA("UnionOperation") or v:IsA("MeshPart") or v:IsA("CornerWedgePart") or v:IsA("TrussPart") then
        v.Material = "Plastic"
        v.Reflectance = 0
      elseif v:IsA("Decal") then
        v.Transparency = 1
      elseif v:IsA("ParticleEmitter") or v:IsA("Trail") then
        v.Lifetime = NumberRange.new(0)
      elseif v:IsA("Explosion") then
        v.BlastPressure = 1
        v.BlastRadius = 1
      end
    end
    for i,v in pairs(Lighting:GetDescendants()) do
      if v:IsA("BlurEffect") or v:IsA("SunRaysEffect") or v:IsA("ColorCorrectionEffect") or v:IsA("BloomEffect") or v:IsA("DepthOfFieldEffect") then
        v.Enabled = false
      end
    end
    workspace.DescendantAdded:Connect(function(child)
      task.spawn(function()
        if child:IsA('ForceField') then
          RunService.Heartbeat:Wait()
          child:Destroy()
        elseif child:IsA('Sparkles') then
          RunService.Heartbeat:Wait()
          child:Destroy()
        elseif child:IsA('Smoke') or child:IsA('Fire') then
          RunService.Heartbeat:Wait()
          child:Destroy()
        end
      end)
    end)
  end)
  
  addcmd('setfpscap', {'fpscap', 'maxfps'}, function(args, speaker)
    if setfpscap and type(setfpscap) == "function" then
      local num = args[1] or 1e6
      if num == 'none' then
        return setfpscap(1e6)
      elseif num > 0 then
        return setfpscap(num)
      else
        return notify('Invalid argument', "Please provide a number above 0 or 'none'.")
      end
    else
      return notify('Incompatible Exploit', 'Your exploit does not support this command (missing setfpscap)')
    end
  end)
  
  addcmd('notify',{},function(args, speaker)
    notify(getstring(1))
  end)
  
  addcmd('lastcommand',{'lastcmd'},function(args, speaker)
    if cmdHistory[1]:sub(1,11) ~= 'lastcommand' and cmdHistory[1]:sub(1,7) ~= 'lastcmd' then
      execCmd(cmdHistory[1])
    end
  end)
  
  addcmd('esp',{},function(args, speaker)
    if not CHMSenabled then
      ESPenabled = true
      for i,v in pairs(Players:GetPlayers()) do
        if v.Name ~= speaker.Name then
          ESP(v)
        end
      end
    else
      notify('ESP','Disable chams (nochams) before using esp')
    end
  end)
  
  addcmd('noesp',{'unesp'},function(args, speaker)
    ESPenabled = false
    for i,c in pairs(COREGUI:GetChildren()) do
      if string.sub(c.Name, -4) == '_ESP' then
        c:Destroy()
      end
    end
  end)
  
  addcmd('esptransparency',{},function(args, speaker)
    espTransparency = (args[1] and isNumber(args[1]) and args[1]) or 0.3
    updatesaves()
  end)
  
  local espParts = {}
  local partEspTrigger = nil
  function partAdded(part)
    if #espParts > 0 then
      if FindInTable(espParts,part.Name:lower()) then
        local a = Instance.new("BoxHandleAdornment")
        a.Name = part.Name:lower().."_PESP"
        a.Parent = part
        a.Adornee = part
        a.AlwaysOnTop = true
        a.ZIndex = 0
        a.Size = part.Size
        a.Transparency = espTransparency
        a.Color = BrickColor.new("Lime green")
      end
    else
      partEspTrigger:Disconnect()
      partEspTrigger = nil
    end
  end
  
  addcmd('partesp',{},function(args, speaker)
    local partEspName = getstring(1):lower()
    if not FindInTable(espParts,partEspName) then
      table.insert(espParts,partEspName)
      for i,v in pairs(workspace:GetDescendants()) do
        if v:IsA("BasePart") and v.Name:lower() == partEspName then
          local a = Instance.new("BoxHandleAdornment")
          a.Name = partEspName.."_PESP"
          a.Parent = v
          a.Adornee = v
          a.AlwaysOnTop = true
          a.ZIndex = 0
          a.Size = v.Size
          a.Transparency = espTransparency
          a.Color = BrickColor.new("Lime green")
        end
      end
    end
    if partEspTrigger == nil then
      partEspTrigger = workspace.DescendantAdded:Connect(partAdded)
    end
  end)
  
  addcmd('unpartesp',{'nopartesp'},function(args, speaker)
    if args[1] then
      local partEspName = getstring(1):lower()
      if FindInTable(espParts,partEspName) then
        table.remove(espParts, GetInTable(espParts, partEspName))
      end
      for i,v in pairs(workspace:GetDescendants()) do
        if v:IsA("BoxHandleAdornment") and v.Name == partEspName..'_PESP' then
          v:Destroy()
        end
      end
    else
      partEspTrigger:Disconnect()
      partEspTrigger = nil
      espParts = {}
      for i,v in pairs(workspace:GetDescendants()) do
        if v:IsA("BoxHandleAdornment") and v.Name:sub(-5) == '_PESP' then
          v:Destroy()
        end
      end
    end
  end)
  
  addcmd('chams',{},function(args, speaker)
    if not ESPenabled then
      CHMSenabled = true
      for i,v in pairs(Players:GetPlayers()) do
        if v.Name ~= speaker.Name then
          CHMS(v)
        end
      end
    else
      notify('Chams','Disable ESP (noesp) before using chams')
    end
  end)
  
  addcmd('nochams',{'unchams'},function(args, speaker)
    CHMSenabled = false
    for i,v in pairs(Players:GetPlayers()) do
      local chmsplr = v
      for i,c in pairs(COREGUI:GetChildren()) do
        if c.Name == chmsplr.Name..'_CHMS' then
          c:Destroy()
        end
      end
    end
  end)
  
  addcmd('locate',{},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      Locate(Players[v])
    end
  end)
  
  addcmd('nolocate',{'unlocate'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    if args[1] then
      for i,v in pairs(players) do
        for i,c in pairs(COREGUI:GetChildren()) do
          if c.Name == Players[v].Name..'_LC' then
            c:Destroy()
          end
        end
      end
    else
      for i,c in pairs(COREGUI:GetChildren()) do
        if string.sub(c.Name, -3) == '_LC' then
          c:Destroy()
        end
      end
    end
  end)
  
  viewing = nil
  addcmd('view',{'spectate'},function(args, speaker)
    StopFreecam()
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      if viewDied then
        viewDied:Disconnect()
        viewChanged:Disconnect()
      end
      viewing = Players[v]
      workspace.CurrentCamera.CameraSubject = viewing.Character
      notify('Spectate','Viewing ' .. Players[v].Name)
      local function viewDiedFunc()
        repeat wait() until Players[v].Character ~= nil and getRoot(Players[v].Character)
        workspace.CurrentCamera.CameraSubject = viewing.Character
      end
      viewDied = Players[v].CharacterAdded:Connect(viewDiedFunc)
      local function viewChangedFunc()
        workspace.CurrentCamera.CameraSubject = viewing.Character
      end
      viewChanged = workspace.CurrentCamera:GetPropertyChangedSignal("CameraSubject"):Connect(viewChangedFunc)
    end
  end)
  
  addcmd('viewpart',{'viewp'},function(args, speaker)
    StopFreecam()
    if args[1] then
      for i,v in pairs(workspace:GetDescendants()) do
        if v.Name:lower() == getstring(1):lower() and v:IsA("BasePart") then
          wait(0.1)
          workspace.CurrentCamera.CameraSubject = v
        end
      end
    end
  end)
  
  addcmd('unview',{'unspectate'},function(args, speaker)
    StopFreecam()
    if viewing ~= nil then
      viewing = nil
      notify('Spectate','View turned off')
    end
    if viewDied then
      viewDied:Disconnect()
      viewChanged:Disconnect()
    end
    workspace.CurrentCamera.CameraSubject = speaker.Character
  end)
  
  
  fcRunning = false
  local Camera = workspace.CurrentCamera
  workspace:GetPropertyChangedSignal("CurrentCamera"):Connect(function()
    local newCamera = workspace.CurrentCamera
    if newCamera then
      Camera = newCamera
    end
  end)
  
  local INPUT_PRIORITY = Enum.ContextActionPriority.High.Value
  
  Spring = {} do
    Spring.__index = Spring
  
    function Spring.new(freq, pos)
      local self = setmetatable({}, Spring)
      self.f = freq
      self.p = pos
      self.v = pos*0
      return self
    end
  
    function Spring:Update(dt, goal)
      local f = self.f*2*math.pi
      local p0 = self.p
      local v0 = self.v
  
      local offset = goal - p0
      local decay = math.exp(-f*dt)
  
      local p1 = goal + (v0*dt - offset*(f*dt + 1))*decay
      local v1 = (f*dt*(offset*f - v0) + v0)*decay
  
      self.p = p1
      self.v = v1
  
      return p1
    end
  
    function Spring:Reset(pos)
      self.p = pos
      self.v = pos*0
    end
  end
  
  local cameraPos = Vector3.new()
  local cameraRot = Vector2.new()
  
  local velSpring = Spring.new(5, Vector3.new())
  local panSpring = Spring.new(5, Vector2.new())
  
  Input = {} do
  
    keyboard = {
      W = 0,
      A = 0,
      S = 0,
      D = 0,
      E = 0,
      Q = 0,
      Up = 0,
      Down = 0,
      LeftShift = 0,
    }
  
    mouse = {
      Delta = Vector2.new(),
    }
  
    NAV_KEYBOARD_SPEED = Vector3.new(1, 1, 1)
    PAN_MOUSE_SPEED = Vector2.new(1, 1)*(math.pi/64)
    NAV_ADJ_SPEED = 0.75
    NAV_SHIFT_MUL = 0.25
  
    navSpeed = 1
  
    function Input.Vel(dt)
      navSpeed = math.clamp(navSpeed + dt*(keyboard.Up - keyboard.Down)*NAV_ADJ_SPEED, 0.01, 4)
  
      local kKeyboard = Vector3.new(
        keyboard.D - keyboard.A,
        keyboard.E - keyboard.Q,
        keyboard.S - keyboard.W
      )*NAV_KEYBOARD_SPEED
  
      local shift = UserInputService:IsKeyDown(Enum.KeyCode.LeftShift)
  
      return (kKeyboard)*(navSpeed*(shift and NAV_SHIFT_MUL or 1))
    end
  
    function Input.Pan(dt)
      local kMouse = mouse.Delta*PAN_MOUSE_SPEED
      mouse.Delta = Vector2.new()
      return kMouse
    end
  
    do
      function Keypress(action, state, input)
        keyboard[input.KeyCode.Name] = state == Enum.UserInputState.Begin and 1 or 0
        return Enum.ContextActionResult.Sink
      end
  
      function MousePan(action, state, input)
        local delta = input.Delta
        mouse.Delta = Vector2.new(-delta.y, -delta.x)
        return Enum.ContextActionResult.Sink
      end
  
      function Zero(t)
        for k, v in pairs(t) do
          t[k] = v*0
        end
      end
  
      function Input.StartCapture()
        ContextActionService:BindActionAtPriority("FreecamKeyboard",Keypress,false,INPUT_PRIORITY,
          Enum.KeyCode.W,
          Enum.KeyCode.A,
          Enum.KeyCode.S,
          Enum.KeyCode.D,
          Enum.KeyCode.E,
          Enum.KeyCode.Q,
          Enum.KeyCode.Up,
          Enum.KeyCode.Down
        )
        ContextActionService:BindActionAtPriority("FreecamMousePan",MousePan,false,INPUT_PRIORITY,Enum.UserInputType.MouseMovement)
      end
  
      function Input.StopCapture()
        navSpeed = 1
        Zero(keyboard)
        Zero(mouse)
        ContextActionService:UnbindAction("FreecamKeyboard")
        ContextActionService:UnbindAction("FreecamMousePan")
      end
    end
  end
  
  function GetFocusDistance(cameraFrame)
    local znear = 0.1
    local viewport = Camera.ViewportSize
    local projy = 2*math.tan(cameraFov/2)
    local projx = viewport.x/viewport.y*projy
    local fx = cameraFrame.rightVector
    local fy = cameraFrame.upVector
    local fz = cameraFrame.lookVector
  
    local minVect = Vector3.new()
    local minDist = 512
  
    for x = 0, 1, 0.5 do
      for y = 0, 1, 0.5 do
        local cx = (x - 0.5)*projx
        local cy = (y - 0.5)*projy
        local offset = fx*cx - fy*cy + fz
        local origin = cameraFrame.p + offset*znear
        local _, hit = workspace:FindPartOnRay(Ray.new(origin, offset.unit*minDist))
        local dist = (hit - origin).magnitude
        if minDist > dist then
          minDist = dist
          minVect = offset.unit
        end
      end
    end
  
    return fz:Dot(minVect)*minDist
  end
  
  local function StepFreecam(dt)
    local vel = velSpring:Update(dt, Input.Vel(dt))
    local pan = panSpring:Update(dt, Input.Pan(dt))
  
    local zoomFactor = math.sqrt(math.tan(math.rad(70/2))/math.tan(math.rad(cameraFov/2)))
  
    cameraRot = cameraRot + pan*Vector2.new(0.75, 1)*8*(dt/zoomFactor)
    cameraRot = Vector2.new(math.clamp(cameraRot.x, -math.rad(90), math.rad(90)), cameraRot.y%(2*math.pi))
  
    local cameraCFrame = CFrame.new(cameraPos)*CFrame.fromOrientation(cameraRot.x, cameraRot.y, 0)*CFrame.new(vel*Vector3.new(1, 1, 1)*64*dt)
    cameraPos = cameraCFrame.p
  
    Camera.CFrame = cameraCFrame
    Camera.Focus = cameraCFrame*CFrame.new(0, 0, -GetFocusDistance(cameraCFrame))
    Camera.FieldOfView = cameraFov
  end
  
  local PlayerState = {} do
    mouseBehavior = ""
    mouseIconEnabled = ""
    cameraType = ""
    cameraFocus = ""
    cameraCFrame = ""
    cameraFieldOfView = ""
  
    function PlayerState.Push()
      cameraFieldOfView = Camera.FieldOfView
      Camera.FieldOfView = 70
  
      cameraType = Camera.CameraType
      Camera.CameraType = Enum.CameraType.Custom
  
      cameraCFrame = Camera.CFrame
      cameraFocus = Camera.Focus
  
      mouseIconEnabled = UserInputService.MouseIconEnabled
      UserInputService.MouseIconEnabled = true
  
      mouseBehavior = UserInputService.MouseBehavior
      UserInputService.MouseBehavior = Enum.MouseBehavior.Default
    end
  
    function PlayerState.Pop()
      Camera.FieldOfView = 70
  
      Camera.CameraType = cameraType
      cameraType = nil
  
      Camera.CFrame = cameraCFrame
      cameraCFrame = nil
  
      Camera.Focus = cameraFocus
      cameraFocus = nil
  
      UserInputService.MouseIconEnabled = mouseIconEnabled
      mouseIconEnabled = nil
  
      UserInputService.MouseBehavior = mouseBehavior
      mouseBehavior = nil
    end
  end
  
  function StartFreecam(pos)
    if fcRunning then
      StopFreecam()
    end
    local cameraCFrame = Camera.CFrame
    if pos then
      cameraCFrame = pos
    end
    cameraRot = Vector2.new()
    cameraPos = cameraCFrame.p
    cameraFov = Camera.FieldOfView
  
    velSpring:Reset(Vector3.new())
    panSpring:Reset(Vector2.new())
  
    PlayerState.Push()
    RunService:BindToRenderStep("Freecam", Enum.RenderPriority.Camera.Value, StepFreecam)
    Input.StartCapture()
    fcRunning = true
  end
  
  function StopFreecam()
    if not fcRunning then return end
    Input.StopCapture()
    RunService:UnbindFromRenderStep("Freecam")
    PlayerState.Pop()
    workspace.Camera.FieldOfView = 70
    fcRunning = false
  end
  
  addcmd('freecam',{'fc'},function(args, speaker)
    StartFreecam()
  end)
  
  addcmd('freecampos',{'fcpos','fcp','freecamposition','fcposition'},function(args, speaker)
    if not args[1] then return end
    local freecamPos = CFrame.new(args[1],args[2],args[3])
    StartFreecam(freecamPos)
  end)
  
  addcmd('freecamwaypoint',{'fcwp'},function(args, speaker)
    local WPName = tostring(getstring(1))
    if speaker.Character then
      for i,_ in pairs(WayPoints) do
        local x = WayPoints[i].COORD[1]
        local y = WayPoints[i].COORD[2]
        local z = WayPoints[i].COORD[3]
        if tostring(WayPoints[i].NAME):lower() == tostring(WPName):lower() then
          StartFreecam(CFrame.new(x,y,z))
        end
      end
      for i,_ in pairs(pWayPoints) do
        if tostring(pWayPoints[i].NAME):lower() == tostring(WPName):lower() then
          StartFreecam(CFrame.new(pWayPoints[i].COORD[1].Position))
        end
      end
    end
  end)
  
  addcmd('freecamgoto',{'fcgoto','freecamtp','fctp'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      StartFreecam(getRoot(Players[v].Character).CFrame)
    end
  end)
  
  addcmd('unfreecam',{'nofreecam','unfc','nofc'},function(args, speaker)
    StopFreecam()
  end)
  
  addcmd('freecamspeed',{'fcspeed'},function(args, speaker)
    local FCspeed = args[1] or 1
    if isNumber(FCspeed) then
      NAV_KEYBOARD_SPEED = Vector3.new(FCspeed, FCspeed, FCspeed)
    end
  end)
  
  addcmd('notifyfreecamposition',{'notifyfcpos'},function(args, speaker)
    if fcRunning then
      local X,Y,Z = workspace.CurrentCamera.CFrame.Position.X,workspace.CurrentCamera.CFrame.Position.Y,workspace.CurrentCamera.CFrame.Position.Z
      local Format, Round = string.format, math.round
      notify("Current Position", Format("%s, %s, %s", Round(X), Round(Y), Round(Z)))
    end
  end)
  
  addcmd('copyfreecamposition',{'copyfcpos'},function(args, speaker)
    if fcRunning then
      local X,Y,Z = workspace.CurrentCamera.CFrame.Position.X,workspace.CurrentCamera.CFrame.Position.Y,workspace.CurrentCamera.CFrame.Position.Z
      local Format, Round = string.format, math.round
      toClipboard(Format("%s, %s, %s", Round(X), Round(Y), Round(Z)))
    end
  end)
  
  addcmd('gotocamera',{'gotocam','tocam'},function(args, speaker)
    getRoot(speaker.Character).CFrame = workspace.Camera.CFrame
  end)
  
  addcmd('tweengotocamera',{'tweengotocam','tgotocam','ttocam'},function(args, speaker)
    TweenService:Create(getRoot(speaker.Character), TweenInfo.new(tweenSpeed, Enum.EasingStyle.Linear), {CFrame = workspace.Camera.CFrame}):Play()
  end)
  
  addcmd('fov',{},function(args, speaker)
    local fov = args[1] or 70
    if isNumber(fov) then
      workspace.CurrentCamera.FieldOfView = fov
    end
  end)
  
  local preMaxZoom = Players.LocalPlayer.CameraMaxZoomDistance
  local preMinZoom = Players.LocalPlayer.CameraMinZoomDistance
  addcmd('lookat',{},function(args, speaker)
    if speaker.CameraMaxZoomDistance ~= 0.5 then
      preMaxZoom = speaker.CameraMaxZoomDistance
      preMinZoom = speaker.CameraMinZoomDistance
    end
    speaker.CameraMaxZoomDistance = 0.5
    speaker.CameraMinZoomDistance = 0.5
    wait()
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      local target = Players[v].Character
      if target and target:FindFirstChild('Head') then
        workspace.CurrentCamera.CFrame = CFrame.new(workspace.CurrentCamera.CFrame.p, target.Head.CFrame.p)
        wait(0.1)
      end
    end
    speaker.CameraMaxZoomDistance = preMaxZoom
    speaker.CameraMinZoomDistance = preMinZoom
  end)
  
  addcmd('fixcam',{'restorecam'},function(args, speaker)
    StopFreecam()
    execCmd('unview')
    workspace.CurrentCamera:remove()
    wait(.1)
    repeat wait() until speaker.Character ~= nil
    workspace.CurrentCamera.CameraSubject = speaker.Character:FindFirstChildWhichIsA('Humanoid')
    workspace.CurrentCamera.CameraType = "Custom"
    speaker.CameraMinZoomDistance = 0.5
    speaker.CameraMaxZoomDistance = 400
    speaker.CameraMode = "Classic"
    speaker.Character.Head.Anchored = false
  end)
  
  addcmd('enableshiftlock',{'enablesl','shiftlock'},function(args, speaker)
    speaker.DevEnableMouseLock = true
    notify('Shiftlock','Shift lock is now available')
  end)
  
  addcmd('firstp',{},function(args, speaker)
    speaker.CameraMode = "LockFirstPerson"
  end)
  
  addcmd('thirdp',{},function(args, speaker)
    speaker.CameraMode = "Classic"
  end)
  
  addcmd('noclipcam', {'nccam'}, function(args, speaker)
    local sc = (debug and debug.setconstant) or setconstant
    local gc = (debug and debug.getconstants) or getconstants
    if not sc or not getgc or not gc then
      return notify('Incompatible Exploit', 'Your exploit does not support this command (missing setconstant or getconstants or getgc)')
    end
    local pop = speaker.PlayerScripts.PlayerModule.CameraModule.ZoomController.Popper
    for _, v in pairs(getgc()) do
      if type(v) == 'function' and getfenv(v).script == pop then
        for i, v1 in pairs(gc(v)) do
          if tonumber(v1) == .25 then
            sc(v, i, 0)
          elseif tonumber(v1) == 0 then
            sc(v, i, .25)
          end
        end
      end
    end
  end)
  
  addcmd('maxzoom',{},function(args, speaker)
    speaker.CameraMaxZoomDistance = args[1]
  end)
  
  addcmd('minzoom',{},function(args, speaker)
    speaker.CameraMinZoomDistance = args[1]
  end)
  
  addcmd('camdistance',{},function(args, speaker)
    local camMax = speaker.CameraMaxZoomDistance
    local camMin = speaker.CameraMinZoomDistance
    if camMax < tonumber(args[1]) then
      camMax = args[1]
    end
    speaker.CameraMaxZoomDistance = args[1]
    speaker.CameraMinZoomDistance = args[1]
    wait()
    speaker.CameraMaxZoomDistance = camMax
    speaker.CameraMinZoomDistance = camMin
  end)
  
  addcmd('unlockws',{'unlockworkspace'},function(args, speaker)
    for i,v in pairs(workspace:GetDescendants()) do
      if v:IsA("BasePart") then
        v.Locked = false
      end
    end
  end)
  
  addcmd('lockws',{'lockworkspace'},function(args, speaker) 
    for i,v in pairs(workspace:GetDescendants()) do
      if v:IsA("BasePart") then
        v.Locked = true
      end
    end
  end)
  
  addcmd('delete',{'remove'},function(args, speaker)
    for i,v in pairs(workspace:GetDescendants()) do
      if v.Name:lower() == getstring(1):lower() then
        v:Destroy()
      end
    end
    notify('Item(s) Deleted','Deleted ' ..getstring(1))
  end)
  
  addcmd('deleteclass',{'removeclass','deleteclassname','removeclassname','dc'},function(args, speaker)
    for i,v in pairs(workspace:GetDescendants()) do
      if v.ClassName:lower() == getstring(1):lower() then
        v:Destroy()
      end
    end
    notify('Item(s) Deleted','Deleted items with ClassName ' ..getstring(1))
  end)
  
  addcmd('chardelete',{'charremove','cd'},function(args, speaker)
    for i,v in pairs(speaker.Character:GetDescendants()) do
      if v.Name:lower() == getstring(1):lower() then
        v:Destroy()
      end
    end
    notify('Item(s) Deleted','Deleted ' ..getstring(1))
  end)
  
  addcmd('chardeleteclass',{'charremoveclass','chardeleteclassname','charremoveclassname','cdc'},function(args, speaker)
    for i,v in pairs(speaker.Character:GetDescendants()) do
      if v.ClassName:lower() == getstring(1):lower() then
        v:Destroy()
      end
    end
    notify('Item(s) Deleted','Deleted items with ClassName ' ..getstring(1))
  end)
  
  addcmd('deletevelocity',{'dv','removevelocity','removeforces'},function(args, speaker)
    for i,v in pairs(speaker.Character:GetDescendants()) do
      if v:IsA("BodyVelocity") or v:IsA("BodyGyro") or v:IsA("RocketPropulsion") or v:IsA("BodyThrust") or v:IsA("BodyAngularVelocity") or v:IsA("AngularVelocity") or v:IsA("BodyForce") or v:IsA("VectorForce") or v:IsA("LineForce") then
        v:Destroy()
      end
    end
  end)
  
  addcmd('deleteinvisparts',{'deleteinvisibleparts','dip'},function(args, speaker)
    for i,v in pairs(workspace:GetDescendants()) do
      if v:IsA("BasePart") and v.Transparency == 1 and v.CanCollide then
        v:Destroy()
      end
    end
  end)
  
  local shownParts = {}
  addcmd('invisibleparts',{'invisparts'},function(args, speaker)
    for i,v in pairs(workspace:GetDescendants()) do
      if v:IsA("BasePart") and v.Transparency == 1 then
        if not table.find(shownParts,v) then
          table.insert(shownParts,v)
        end
        v.Transparency = 0
      end
    end
  end)
  
  addcmd('uninvisibleparts',{'uninvisparts'},function(args, speaker)
    for i,v in pairs(shownParts) do
      v.Transparency = 1
    end
    shownParts = {}
  end)
  
  addcmd('btools',{},function(args, speaker)
    for i = 1, 4 do
      local Tool = Instance.new("HopperBin")
      Tool.BinType = i
      Tool.Name = randomString()
      Tool.Parent = speaker:FindFirstChildOfClass("Backpack")
    end
  end)
  
  addcmd('f3x',{'fex'},function(args, speaker)
    loadstring(game:GetObjects("rbxassetid://6695644299")[1].Source)()
  end)
  
  addcmd('partpath',{'partname'},function(args, speaker)
    selectPart()
  end)
  
  addcmd('antiafk',{'antiidle'},function(args, speaker)
    local GC = getconnections or get_signal_cons
    if GC then
      for i,v in pairs(GC(Players.LocalPlayer.Idled)) do
        if v["Disable"] then
          v["Disable"](v)
        elseif v["Disconnect"] then
          v["Disconnect"](v)
        end
      end
    else
      local VirtualUser = cloneref(game:GetService("VirtualUser"))
      Players.LocalPlayer.Idled:Connect(function()
        VirtualUser:CaptureController()
        VirtualUser:ClickButton2(Vector2.new())
      end)
    end
    if not (args[1] and tostring(args[1]) == 'nonotify') then notify('Anti Idle','Anti idle is enabled') end
  end)
  
  addcmd("datalimit", {}, function(args, speaker)
    if tonumber(args[1]) then
      NetworkClient:SetOutgoingKBPSLimit(args[1])
    end
  end)
  
  addcmd("replicationlag", {"backtrack"}, function(args, speaker)
    if tonumber(args[1]) then
      settings():GetService("NetworkSettings").IncomingReplicationLag = args[1]
    end
  end)
  
  addcmd("noprompts", {"nopurchaseprompts"}, function(args, speaker)
    COREGUI.PurchasePrompt.Enabled = false
  end)
  
  addcmd("showprompts", {"showpurchaseprompts"}, function(args, speaker)
    COREGUI.PurchasePrompt.Enabled = true
  end)
  
  promptNewRig = function(speaker, rig)
    local humanoid = speaker.Character:FindFirstChildWhichIsA("Humanoid")
    if humanoid then
      AvatarEditorService:PromptSaveAvatar(humanoid.HumanoidDescription, Enum.HumanoidRigType[rig])
      local result = AvatarEditorService.PromptSaveAvatarCompleted:Wait()
      if result == Enum.AvatarPromptResult.Success then
        execCmd("reset")
      end
    end
  end
  
  addcmd("promptr6", {}, function(args, speaker)
    promptNewRig(speaker, "R6")
  end)
  
  addcmd("promptr15", {}, function(args, speaker)
    promptNewRig(speaker, "R15")
  end)
  
  addcmd("wallwalk", {"walkonwalls"}, function(args, speaker)
      loadstring(game:HttpGet("https://raw.githubusercontent.com/infyiff/backup/main/wallwalker.lua"))()
  end)
  
  addcmd('age',{},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    local ages = {}
    for i,v in pairs(players) do
      local p = Players[v]
      table.insert(ages, p.Name.."'s age is: "..p.AccountAge)
    end
    notify('Account Age',table.concat(ages, ',\n'))
  end)
  
  addcmd('chatage',{},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    local ages = {}
    for i,v in pairs(players) do
      local p = Players[v]
      table.insert(ages, p.Name.."'s age is: "..p.AccountAge)
    end
    local chatString = table.concat(ages, ', ')
    chatMessage(chatString)
  end)
  
  addcmd('joindate',{'jd'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    local dates = {}
    notify("Loading",'Hold on a sec')
    for i,v in pairs(players) do
      local user = game:HttpGet("https://users.roblox.com/v1/users/"..Players[v].UserId)
      local json = HttpService:JSONDecode(user)
      local date = json["created"]:sub(1,10)
      local splitDates = string.split(date,"-")
      table.insert(dates,Players[v].Name.." joined: "..splitDates[2].."/"..splitDates[3].."/"..splitDates[1])
    end
    notify('Join Date (Month/Day/Year)',table.concat(dates, ',\n'))
  end)
  
  addcmd('chatjoindate',{'cjd'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    local dates = {}
    notify("Loading",'Hold on a sec')
    for i,v in pairs(players) do
      local user = game:HttpGet("https://users.roblox.com/v1/users/"..Players[v].UserId)
      local json = HttpService:JSONDecode(user)
      local date = json["created"]:sub(1,10)
      local splitDates = string.split(date,"-")
      table.insert(dates,Players[v].Name.." joined: "..splitDates[2].."/"..splitDates[3].."/"..splitDates[1])
    end
    local chatString = table.concat(dates, ', ')
    chatMessage(chatString)
  end)
  
  addcmd('copyname',{'copyuser'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      local name = tostring(Players[v].Name)
      toClipboard(name)
    end
  end)
  
  addcmd('userid',{'id'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      local id = tostring(Players[v].UserId)
      notify('User ID',id)
    end
  end)
  
  addcmd('copyid',{'copyuserid'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      local id = tostring(Players[v].UserId)
      toClipboard(id)
    end
  end)
  
  addcmd('creatorid',{'creator'},function(args, speaker)
    if game.CreatorType == Enum.CreatorType.User then
      notify('Creator ID',game.CreatorId)
    elseif game.CreatorType == Enum.CreatorType.Group then
      local OwnerID = GroupService:GetGroupInfoAsync(game.CreatorId).Owner.Id
      speaker.UserId = OwnerID
      notify('Creator ID',OwnerID)
    end
  end)
  
  addcmd('copycreatorid',{'copycreator'},function(args, speaker)
    if game.CreatorType == Enum.CreatorType.User then
      toClipboard(game.CreatorId)
      notify('Copied ID','Copied creator ID to clipboard')
    elseif game.CreatorType == Enum.CreatorType.Group then
      local OwnerID = GroupService:GetGroupInfoAsync(game.CreatorId).Owner.Id
      toClipboard(OwnerID)
      notify('Copied ID','Copied creator ID to clipboard')
    end
  end)
  
  addcmd('setcreatorid',{'setcreator'},function(args, speaker)
    if game.CreatorType == Enum.CreatorType.User then
      speaker.UserId = game.CreatorId
      notify('Set ID','Set UserId to '..game.CreatorId)
    elseif game.CreatorType == Enum.CreatorType.Group then
      local OwnerID = GroupService:GetGroupInfoAsync(game.CreatorId).Owner.Id
      speaker.UserId = OwnerID
      notify('Set ID','Set UserId to '..OwnerID)
    end
  end)
  
  addcmd('appearanceid',{'aid'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      local aid = tostring(Players[v].CharacterAppearanceId)
      notify('Appearance ID',aid)
    end
  end)
  
  addcmd('copyappearanceid',{'caid'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      local aid = tostring(Players[v].CharacterAppearanceId)
      toClipboard(aid)
    end
  end)
  
  addcmd('norender',{},function(args, speaker)
    RunService:Set3dRenderingEnabled(false)
  end)
  
  addcmd('render',{},function(args, speaker)
    RunService:Set3dRenderingEnabled(true)
  end)
  
  addcmd('2022materials',{'use2022materials'},function(args, speaker)
    if sethidden then
      sethidden(MaterialService, "Use2022Materials", true)
    else
      notify('Incompatible Exploit','Your exploit does not support this command (missing sethiddenproperty)')
    end
  end)
  
  addcmd('un2022materials',{'unuse2022materials'},function(args, speaker)
    if sethidden then
      sethidden(MaterialService, "Use2022Materials", false)
    else
      notify('Incompatible Exploit','Your exploit does not support this command (missing sethiddenproperty)')
    end
  end)
  
  addcmd('goto',{'to'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      if Players[v].Character ~= nil then
        if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
          speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
          wait(.1)
        end
        getRoot(speaker.Character).CFrame = getRoot(Players[v].Character).CFrame + Vector3.new(3,1,0)
      end
    end
    execCmd('breakvelocity')
  end)
  
  addcmd('tweengoto',{'tgoto','tto','tweento'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      if Players[v].Character ~= nil then
        if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
          speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
          wait(.1)
        end
        TweenService:Create(getRoot(speaker.Character), TweenInfo.new(tweenSpeed, Enum.EasingStyle.Linear), {CFrame = getRoot(Players[v].Character).CFrame + Vector3.new(3,1,0)}):Play()
      end
    end
    execCmd('breakvelocity')
  end)
  
  addcmd('vehiclegoto',{'vgoto','vtp','vehicletp'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      if Players[v].Character ~= nil then
        local seat = speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart
        local vehicleModel = seat:FindFirstAncestorWhichIsA("Model")
        vehicleModel:MoveTo(getRoot(Players[v].Character).Position)
      end
    end
  end)
  
  addcmd('pulsetp',{'ptp'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      if Players[v].Character ~= nil then
        local startPos = getRoot(speaker.Character).CFrame
        local seconds = args[2] or 1
        if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
          speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
          wait(.1)
        end
        getRoot(speaker.Character).CFrame = getRoot(Players[v].Character).CFrame + Vector3.new(3,1,0)
        wait(seconds)
        getRoot(speaker.Character).CFrame = startPos
      end
    end
    execCmd('breakvelocity')
  end)
  
  local vnoclipParts = {}
  addcmd('vehiclenoclip',{'vnoclip'},function(args, speaker)
    vnoclipParts = {}
    local seat = speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart
    local vehicleModel = seat.Parent
    repeat
      if vehicleModel.ClassName ~= "Model" then
        vehicleModel = vehicleModel.Parent
      end
    until vehicleModel.ClassName == "Model"
    wait(0.1)
    execCmd('noclip')
    for i,v in pairs(vehicleModel:GetDescendants()) do
      if v:IsA("BasePart") and v.CanCollide then
        table.insert(vnoclipParts,v)
        v.CanCollide = false
      end
    end
  end)
  
  addcmd("vehicleclip", {"vclip", "unvnoclip", "unvehiclenoclip"}, function(args, speaker)
    execCmd("clip")
    for i, v in pairs(vnoclipParts) do
      v.CanCollide = true
    end
    vnoclipParts = {}
  end)
  
  addcmd("togglevnoclip", {}, function(args, speaker)
    execCmd(Clip and "vnoclip" or "vclip")
  end)
  
  addcmd('clientbring',{'cbring'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      if Players[v].Character ~= nil then
        if Players[v].Character:FindFirstChildOfClass('Humanoid') then
          Players[v].Character:FindFirstChildOfClass('Humanoid').Sit = false
        end
        wait()
        getRoot(Players[v].Character).CFrame = getRoot(speaker.Character).CFrame + Vector3.new(3,1,0)
      end
    end
  end)
  
  local bringT = {}
  addcmd('loopbring',{},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      task.spawn(function()
        if Players[v].Name ~= speaker.Name and not FindInTable(bringT, Players[v].Name) then
          table.insert(bringT, Players[v].Name)
          local plrName = Players[v].Name
          local pchar=Players[v].Character
          local distance = 3
          if args[2] and isNumber(args[2]) then
            distance = args[2]
          end
          local lDelay = 0
          if args[3] and isNumber(args[3]) then
            lDelay = args[3]
          end
          repeat
            for i,c in pairs(players) do
              if Players:FindFirstChild(v) then
                pchar = Players[v].Character
                if pchar~= nil and Players[v].Character ~= nil and getRoot(pchar) and speaker.Character ~= nil and getRoot(speaker.Character) then
                  getRoot(pchar).CFrame = getRoot(speaker.Character).CFrame + Vector3.new(distance,1,0)
                end
                wait(lDelay)
              else 
                for a,b in pairs(bringT) do if b == plrName then table.remove(bringT, a) end end
              end
            end
          until not FindInTable(bringT, plrName)
        end
      end)
    end
  end)
  
  addcmd('unloopbring',{'noloopbring'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      task.spawn(function()
        for a,b in pairs(bringT) do if b == Players[v].Name then table.remove(bringT, a) end end
      end)
    end
  end)
  
  local walkto = false
  local waypointwalkto = false
  addcmd('walkto',{'follow'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      if Players[v].Character ~= nil then
        if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
          speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
          wait(.1)
        end
        walkto = true
        repeat wait()
          speaker.Character:FindFirstChildOfClass('Humanoid'):MoveTo(getRoot(Players[v].Character).Position)
        until Players[v].Character == nil or not getRoot(Players[v].Character) or walkto == false
      end
    end
  end)
  
  addcmd('pathfindwalkto',{'pathfindfollow'},function(args, speaker)
    walkto = false
    wait()
    local players = getPlayer(args[1], speaker)
    local hum = Players.LocalPlayer.Character:FindFirstChildOfClass("Humanoid")
    local path = PathService:CreatePath()
    for i,v in pairs(players)do
      if Players[v].Character ~= nil then
        if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
          speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
          wait(.1)
        end
        walkto = true
        repeat wait()
          local success, response = pcall(function()
            path:ComputeAsync(getRoot(speaker.Character).Position, getRoot(Players[v].Character).Position)
            local waypoints = path:GetWaypoints()
            local distance 
            for waypointIndex, waypoint in pairs(waypoints) do
              local waypointPosition = waypoint.Position
              hum:MoveTo(waypointPosition)
              repeat 
                distance = (waypointPosition - hum.Parent.PrimaryPart.Position).magnitude
                wait()
              until
              distance <= 5
            end	 
          end)
          if not success then
            speaker.Character:FindFirstChildOfClass('Humanoid'):MoveTo(getRoot(Players[v].Character).Position)
          end
        until Players[v].Character == nil or not getRoot(Players[v].Character) or walkto == false
      end
    end
  end)
  
  addcmd('pathfindwalktowaypoint',{'pathfindwalktowp'},function(args, speaker)
    waypointwalkto = false
    wait()
    local WPName = tostring(getstring(1))
    local hum = Players.LocalPlayer.Character:FindFirstChildOfClass("Humanoid")
    local path = PathService:CreatePath()
    if speaker.Character then
      for i,_ in pairs(WayPoints) do
        if tostring(WayPoints[i].NAME):lower() == tostring(WPName):lower() then
          if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
            speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
            wait(.1)
          end
          local TrueCoords = Vector3.new(WayPoints[i].COORD[1], WayPoints[i].COORD[2], WayPoints[i].COORD[3])
          waypointwalkto = true
          repeat wait()
            local success, response = pcall(function()
              path:ComputeAsync(getRoot(speaker.Character).Position, TrueCoords)
              local waypoints = path:GetWaypoints()
              local distance 
              for waypointIndex, waypoint in pairs(waypoints) do
                local waypointPosition = waypoint.Position
                hum:MoveTo(waypointPosition)
                repeat 
                  distance = (waypointPosition - hum.Parent.PrimaryPart.Position).magnitude
                  wait()
                until
                distance <= 5
              end
            end)
            if not success then
              speaker.Character:FindFirstChildOfClass('Humanoid'):MoveTo(TrueCoords)
            end
          until not speaker.Character or waypointwalkto == false
        end
      end
      for i,_ in pairs(pWayPoints) do
        if tostring(pWayPoints[i].NAME):lower() == tostring(WPName):lower() then
          if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
            speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
            wait(.1)
          end
          local TrueCoords = pWayPoints[i].COORD[1].Position
          waypointwalkto = true
          repeat wait()
            local success, response = pcall(function()
              path:ComputeAsync(getRoot(speaker.Character).Position, TrueCoords)
              local waypoints = path:GetWaypoints()
              local distance 
              for waypointIndex, waypoint in pairs(waypoints) do
                local waypointPosition = waypoint.Position
                hum:MoveTo(waypointPosition)
                repeat 
                  distance = (waypointPosition - hum.Parent.PrimaryPart.Position).magnitude
                  wait()
                until
                distance <= 5
              end
            end)
            if not success then
              speaker.Character:FindFirstChildOfClass('Humanoid'):MoveTo(TrueCoords)
            end
          until not speaker.Character or waypointwalkto == false
        end
      end
    end
  end)
  
  addcmd('unwalkto',{'nowalkto','unfollow','nofollow'},function(args, speaker)
    walkto = false
    waypointwalkto = false
  end)
  
  addcmd('freeze',{'fr'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    if players ~= nil then
      for i,v in pairs(players) do
        task.spawn(function()
          for i, x in next, Players[v].Character:GetDescendants() do
            if x:IsA("BasePart") and not x.Anchored then
              x.Anchored = true
            end
          end
        end)
      end
    end
  end)
  
  
  addcmd('thaw',{'unfreeze','unfr'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    if players ~= nil then
      for i,v in pairs(players) do
        task.spawn(function()
          for i, x in next, Players[v].Character:GetDescendants() do
            if x.Name ~= floatName and x:IsA("BasePart") and x.Anchored then
              x.Anchored = false
            end
          end
        end)
      end
    end
  end)
  
  oofing = false
  addcmd('loopoof',{},function(args, speaker)
    oofing = true
    repeat wait(0.1)
      for i,v in pairs(Players:GetPlayers()) do
        if v.Character ~= nil and v.Character:FindFirstChild'Head' then
          for _,x in pairs(v.Character.Head:GetChildren()) do
            if x:IsA'Sound' then x.Playing = true end
          end
        end
      end
    until oofing == false
  end)
  
  addcmd('unloopoof',{},function(args, speaker)
    oofing = false
  end)
  
  local notifiedRespectFiltering = false
  addcmd('muteboombox',{},function(args, speaker)
    if not notifiedRespectFiltering and SoundService.RespectFilteringEnabled then notifiedRespectFiltering = true notify('RespectFilteringEnabled','RespectFilteringEnabled is set to true (the command will still work but may only be clientsided)') end
    local players = getPlayer(args[1], speaker)
    if players ~= nil then
      for i,v in pairs(players) do
        task.spawn(function()
          for i, x in next, Players[v].Character:GetDescendants() do
            if x:IsA("Sound") and x.Playing == true then
              x.Playing = false
            end
          end
          for i, x in next, Players[v]:FindFirstChildOfClass("Backpack"):GetDescendants() do
            if x:IsA("Sound") and x.Playing == true then
              x.Playing = false
            end
          end
        end)
      end
    end
  end)
  
  addcmd('unmuteboombox',{},function(args, speaker)
    if not notifiedRespectFiltering and SoundService.RespectFilteringEnabled then notifiedRespectFiltering = true notify('RespectFilteringEnabled','RespectFilteringEnabled is set to true (the command will still work but may only be clientsided)') end
    local players = getPlayer(args[1], speaker)
    if players ~= nil then
      for i,v in pairs(players) do
        task.spawn(function()
          for i, x in next, Players[v].Character:GetDescendants() do
            if x:IsA("Sound") and x.Playing == false then
              x.Playing = true
            end
          end
        end)
      end
    end
  end)
  
  addcmd('reset',{},function(args, speaker)
    speaker.Character:FindFirstChildOfClass("Humanoid"):ChangeState(Enum.HumanoidStateType.Dead)
  end)
  
  addcmd('freezeanims',{},function(args, speaker)
    local Humanoid = speaker.Character:FindFirstChildOfClass("Humanoid") or speaker.Character:FindFirstChildOfClass("AnimationController")
    local ActiveTracks = Humanoid:GetPlayingAnimationTracks()
    for _, v in pairs(ActiveTracks) do
      v:AdjustSpeed(0)
    end
  end)
  
  addcmd('unfreezeanims',{},function(args, speaker)
    local Humanoid = speaker.Character:FindFirstChildOfClass("Humanoid") or speaker.Character:FindFirstChildOfClass("AnimationController")
    local ActiveTracks = Humanoid:GetPlayingAnimationTracks()
    for _, v in pairs(ActiveTracks) do
      v:AdjustSpeed(1)
    end
  end)
  
  
  
  
  addcmd('respawn',{},function(args, speaker)
    respawn(speaker)
  end)
  
  addcmd('refresh',{'re'},function(args, speaker)
    refresh(speaker)
  end)
  
  addcmd('god',{},function(args, speaker)
    local Cam = workspace.CurrentCamera
    local Pos, Char = Cam.CFrame, speaker.Character
    local Human = Char and Char.FindFirstChildWhichIsA(Char, "Humanoid")
    local nHuman = Human.Clone(Human)
    nHuman.Parent, speaker.Character = Char, nil
    nHuman.SetStateEnabled(nHuman, 15, false)
    nHuman.SetStateEnabled(nHuman, 1, false)
    nHuman.SetStateEnabled(nHuman, 0, false)
    nHuman.BreakJointsOnDeath, Human = true, Human.Destroy(Human)
    speaker.Character, Cam.CameraSubject, Cam.CFrame = Char, nHuman, wait() and Pos
    nHuman.DisplayDistanceType = Enum.HumanoidDisplayDistanceType.None
    local Script = Char.FindFirstChild(Char, "Animate")
    if Script then
      Script.Disabled = true
      wait()
      Script.Disabled = false
    end
    nHuman.Health = nHuman.MaxHealth
  end)
  
  invisRunning = false
  addcmd('invisible',{'invis'},function(args, speaker)
    if invisRunning then return end
    invisRunning = true
    -- Full credit to AmokahFox @V3rmillion
    local Player = speaker
    repeat wait(.1) until Player.Character
    local Character = Player.Character
    Character.Archivable = true
    local IsInvis = false
    local IsRunning = true
    local InvisibleCharacter = Character:Clone()
    InvisibleCharacter.Parent = Lighting
    local Void = workspace.FallenPartsDestroyHeight
    InvisibleCharacter.Name = ""
    local CF
  
    local invisFix = RunService.Stepped:Connect(function()
      pcall(function()
        local IsInteger
        if tostring(Void):find'-' then
          IsInteger = true
        else
          IsInteger = false
        end
        local Pos = Player.Character.HumanoidRootPart.Position
        local Pos_String = tostring(Pos)
        local Pos_Seperate = Pos_String:split(', ')
        local X = tonumber(Pos_Seperate[1])
        local Y = tonumber(Pos_Seperate[2])
        local Z = tonumber(Pos_Seperate[3])
        if IsInteger == true then
          if Y <= Void then
            Respawn()
          end
        elseif IsInteger == false then
          if Y >= Void then
            Respawn()
          end
        end
      end)
    end)
  
    for i,v in pairs(InvisibleCharacter:GetDescendants())do
      if v:IsA("BasePart") then
        if v.Name == "HumanoidRootPart" then
          v.Transparency = 1
        else
          v.Transparency = .5
        end
      end
    end
  
    function Respawn()
      IsRunning = false
      if IsInvis == true then
        pcall(function()
          Player.Character = Character
          wait()
          Character.Parent = workspace
          Character:FindFirstChildWhichIsA'Humanoid':Destroy()
          IsInvis = false
          InvisibleCharacter.Parent = nil
          invisRunning = false
        end)
      elseif IsInvis == false then
        pcall(function()
          Player.Character = Character
          wait()
          Character.Parent = workspace
          Character:FindFirstChildWhichIsA'Humanoid':Destroy()
          TurnVisible()
        end)
      end
    end
  
    local invisDied
    invisDied = InvisibleCharacter:FindFirstChildOfClass'Humanoid'.Died:Connect(function()
      Respawn()
      invisDied:Disconnect()
    end)
  
    if IsInvis == true then return end
    IsInvis = true
    CF = workspace.CurrentCamera.CFrame
    local CF_1 = Player.Character.HumanoidRootPart.CFrame
    Character:MoveTo(Vector3.new(0,math.pi*1000000,0))
    workspace.CurrentCamera.CameraType = Enum.CameraType.Scriptable
    wait(.2)
    workspace.CurrentCamera.CameraType = Enum.CameraType.Custom
    InvisibleCharacter = InvisibleCharacter
    Character.Parent = Lighting
    InvisibleCharacter.Parent = workspace
    InvisibleCharacter.HumanoidRootPart.CFrame = CF_1
    Player.Character = InvisibleCharacter
    execCmd('fixcam')
    Player.Character.Animate.Disabled = true
    Player.Character.Animate.Disabled = false
  
    function TurnVisible()
      if IsInvis == false then return end
      invisFix:Disconnect()
      invisDied:Disconnect()
      CF = workspace.CurrentCamera.CFrame
      Character = Character
      local CF_1 = Player.Character.HumanoidRootPart.CFrame
      Character.HumanoidRootPart.CFrame = CF_1
      InvisibleCharacter:Destroy()
      Player.Character = Character
      Character.Parent = workspace
      IsInvis = false
      Player.Character.Animate.Disabled = true
      Player.Character.Animate.Disabled = false
      invisDied = Character:FindFirstChildOfClass'Humanoid'.Died:Connect(function()
        Respawn()
        invisDied:Disconnect()
      end)
      invisRunning = false
    end
    notify('Invisible','You now appear invisible to other players')
  end)
  
  addcmd("visible", {"vis"}, function(args, speaker)
    TurnVisible()
  end)
  
  addcmd("toggleinvis", {}, function(args, speaker)
    execCmd(invisRunning and "visible" or "invisible")
  end)
  
  addcmd('toolinvisible',{'toolinvis','tinvis'},function(args, speaker)
    local Char  = Players.LocalPlayer.Character
    local touched = false
    local tpdback = false
    local box = Instance.new('Part')
    box.Anchored = true
    box.CanCollide = true
    box.Size = Vector3.new(10,1,10)
    box.Position = Vector3.new(0,10000,0)
    box.Parent = workspace
    local boxTouched = box.Touched:connect(function(part)
      if (part.Parent.Name == Players.LocalPlayer.Name) then
        if touched == false then
          touched = true
          local function apply()
            local no = Char.HumanoidRootPart:Clone()
            wait(.25)
            Char.HumanoidRootPart:Destroy()
            no.Parent = Char
            Char:MoveTo(loc)
            touched = false
          end
          if Char then
            apply()
          end
        end
      end
    end)
    repeat wait() until Char
    local cleanUp
    cleanUp = Players.LocalPlayer.CharacterAdded:connect(function(char)
      boxTouched:Disconnect()
      box:Destroy()
      cleanUp:Disconnect()
    end)
    loc = Char.HumanoidRootPart.Position
    Char:MoveTo(box.Position + Vector3.new(0,.5,0))
  end)
  
  addcmd("strengthen", {}, function(args, speaker)
    for _, child in pairs(speaker.Character:GetDescendants()) do
      if child.ClassName == "Part" then
        if args[1] then
          child.CustomPhysicalProperties = PhysicalProperties.new(args[1], 0.3, 0.5)
        else
          child.CustomPhysicalProperties = PhysicalProperties.new(100, 0.3, 0.5)
        end
      end
    end
  end)
  
  addcmd("weaken", {}, function(args, speaker)
    for _, child in pairs(speaker.Character:GetDescendants()) do
      if child.ClassName == "Part" then
        if args[1] then
          child.CustomPhysicalProperties = PhysicalProperties.new(-args[1], 0.3, 0.5)
        else
          child.CustomPhysicalProperties = PhysicalProperties.new(0, 0.3, 0.5)
        end
      end
    end
  end)
  
  addcmd("unweaken", {"unstrengthen"}, function(args, speaker)
    for _, child in pairs(speaker.Character:GetDescendants()) do
      if child.ClassName == "Part" then
        child.CustomPhysicalProperties = PhysicalProperties.new(0.7, 0.3, 0.5)
      end
    end
  end)
  
  addcmd("breakvelocity", {}, function(args, speaker)
    local BeenASecond, V3 = false, Vector3.new(0, 0, 0)
    delay(1, function()
      BeenASecond = true
    end)
    while not BeenASecond do
      for _, v in ipairs(speaker.Character:GetDescendants()) do
        if v:IsA("BasePart") then
          v.Velocity, v.RotVelocity = V3, V3
        end
      end
      wait()
    end
  end)
  
  addcmd('jpower',{'jumppower','jp'},function(args, speaker)
    local jpower = args[1] or 50
    if isNumber(jpower) then
      if speaker.Character:FindFirstChildOfClass('Humanoid').UseJumpPower then
        speaker.Character:FindFirstChildOfClass('Humanoid').JumpPower = jpower
      else
        speaker.Character:FindFirstChildOfClass('Humanoid').JumpHeight  = jpower
      end
    end
  end)
  
  addcmd("maxslopeangle", {"msa"}, function(args, speaker)
    local sangle = args[1] or 89
    if isNumber(sangle) then
      speaker.Character:FindFirstChildWhichIsA("Humanoid").MaxSlopeAngle = sangle
    end
  end)
  
  addcmd("gravity", {"grav"}, function(args, speaker)
    local grav = args[1] or 196.2
    if isNumber(grav) then
      workspace.Gravity = grav
    end
  end)
  
  addcmd("hipheight", {"hheight"}, function(args, speaker)
    speaker.Character:FindFirstChildWhichIsA("Humanoid").HipHeight = args[1] or (r15(speaker) and 2.1 or 0)
  end)
  
  addcmd("dance", {}, function(args, speaker)
    pcall(execCmd, "undance")
    local dances = {"27789359", "30196114", "248263260", "45834924", "33796059", "28488254", "52155728"}
    if r15(speaker) then
      dances = {"3333432454", "4555808220", "4049037604", "4555782893", "10214311282", "10714010337", "10713981723", "10714372526", "10714076981", "10714392151", "11444443576"}
    end
    local animation = Instance.new("Animation")
    animation.AnimationId = "rbxassetid://" .. dances[math.random(1, #dances)]
    danceTrack = speaker.Character:FindFirstChildWhichIsA("Humanoid"):LoadAnimation(animation)
    danceTrack.Looped = true
    danceTrack:Play()
  end)
  
  addcmd("undance", {"nodance"}, function(args, speaker)
    danceTrack:Stop()
    danceTrack:Destroy()
  end)
  
  addcmd('nolimbs',{'rlimbs'},function(args, speaker)
    if r15(speaker) then
      for i,v in pairs(speaker.Character:GetChildren()) do
        if v:IsA("BasePart") and
          v.Name == "RightUpperLeg" or
          v.Name == "LeftUpperLeg" or
          v.Name == "RightUpperArm" or
          v.Name == "LeftUpperArm" then
          v:Destroy()
        end
      end
    else
      for i,v in pairs(speaker.Character:GetChildren()) do
        if v:IsA("BasePart") and
          v.Name == "Right Leg" or
          v.Name == "Left Leg" or
          v.Name == "Right Arm" or
          v.Name == "Left Arm" then
          v:Destroy()
        end
      end
    end
  end)
  
  addcmd('nohead',{'rhead','headless'},function(args, speaker)
    if sethidden then
      -- Full credit to Thomas_Cornez#0272 @Discord
      local lplr = Players.LocalPlayer
      local char = lplr.Character
      local rigType = tostring(char:FindFirstChildOfClass('Humanoid').RigType) == "Enum.HumanoidRigType.R6" and 1 or tostring(char:FindFirstChildOfClass('Humanoid').RigType) == "Enum.HumanoidRigType.R15" and 2
  
      local speaker = Players.LocalPlayer
  
  
      local test = Instance.new("Model")
      local hum  = Instance.new("Humanoid")
      local animation = Instance.new("Model")
      local humanoidanimation = Instance.new("Humanoid")
      test.Parent = workspace
      hum.Parent = test
      animation.Parent = workspace
      humanoidanimation.Parent = animation
  
      lplr.Character = test
      wait(2)
      char:FindFirstChildOfClass('Humanoid').Animator.Parent = humanoidanimation
      char:FindFirstChildOfClass('Humanoid'):Destroy()
  
      char.Head:Destroy()
      wait(5)
      Players.LocalPlayer.Character = char
  
      local hum2 = Instance.new("Humanoid")
      hum2.Parent = char
      char:FindFirstChildOfClass("Humanoid").Jump = true
  
      humanoidanimation.Animator.Parent = hum2
      char.Animate.Disabled = true
      wait()
      char.Animate.Disabled = false
      wait()
  
      if rig == 1 then
        hum2.HipHeight = 0
      elseif rig == 2 then
        hum2.HipHeight = 2.19
      end
    else
      notify('Incompatible Exploit','Your exploit does not support this command (missing sethiddenproperty)')
    end
  end)
  
  addcmd('noarms',{'rarms'},function(args, speaker)
    if r15(speaker) then
      for i,v in pairs(speaker.Character:GetChildren()) do
        if v:IsA("BasePart") and
          v.Name == "RightUpperArm" or
          v.Name == "LeftUpperArm" then
          v:Destroy()
        end
      end
    else
      for i,v in pairs(speaker.Character:GetChildren()) do
        if v:IsA("BasePart") and
          v.Name == "Right Arm" or
          v.Name == "Left Arm" then
          v:Destroy()
        end
      end
    end
  end)
  
  addcmd('nolegs',{'rlegs'},function(args, speaker)
    if r15(speaker) then
      for i,v in pairs(speaker.Character:GetChildren()) do
        if v:IsA("BasePart") and
          v.Name == "RightUpperLeg" or
          v.Name == "LeftUpperLeg" then
          v:Destroy()
        end
      end
    else
      for i,v in pairs(speaker.Character:GetChildren()) do
        if v:IsA("BasePart") and
          v.Name == "Right Leg" or
          v.Name == "Left Leg" then
          v:Destroy()
        end
      end
    end
  end)
  
  addcmd("sit", {}, function(args, speaker)
    speaker.Character:FindFirstChildWhichIsA("Humanoid").Sit = true
  end)
  
  addcmd("lay", {"laydown"}, function(args, speaker)
    local humanoid = speaker.Character:FindFirstChildWhichIsA("Humanoid")
    humanoid.Sit = true
    task.wait(0.1)
    humanoid.RootPart.CFrame = humanoid.RootPart.CFrame * CFrame.Angles(math.pi * 0.5, 0, 0)
    for _, v in ipairs(humanoid:GetPlayingAnimationTracks()) do
      v:Stop()
    end
  end)
  
  addcmd("sitwalk", {}, function(args, speaker)
    local anims = speaker.Character.Animate
    local sit = anims.sit:FindFirstChildWhichIsA("Animation").AnimationId
    anims.idle:FindFirstChildWhichIsA("Animation").AnimationId = sit
    anims.walk:FindFirstChildWhichIsA("Animation").AnimationId = sit
    anims.run:FindFirstChildWhichIsA("Animation").AnimationId = sit
    anims.jump:FindFirstChildWhichIsA("Animation").AnimationId = sit
    speaker.Character:FindFirstChildWhichIsA("Humanoid").HipHeight = not r15(speaker) and -1.5 or 0.5
  end)
  
  function noSitFunc()
    wait()
    if Players.LocalPlayer.Character:FindFirstChildWhichIsA("Humanoid").Sit then
      Players.LocalPlayer.Character:FindFirstChildWhichIsA("Humanoid").Sit = false
    end
  end
  addcmd("nosit", {}, function(args, speaker)
    if noSit then noSit:Disconnect() nositDied:Disconnect() end
    noSit = Players.LocalPlayer.Character:FindFirstChildOfClass('Humanoid'):GetPropertyChangedSignal("Sit"):Connect(noSitFunc)
    local function nositDiedFunc()
      repeat wait() until speaker.Character ~= nil and speaker.Character:FindFirstChildOfClass("Humanoid")
      noSit:Disconnect()
      noSit = Players.LocalPlayer.Character:FindFirstChildOfClass('Humanoid'):GetPropertyChangedSignal("Sit"):Connect(noSitFunc)
    end
    nositDied = speaker.CharacterAdded:Connect(nositDiedFunc)
  end)
  
  addcmd("unnosit", {}, function(args, speaker)
    if noSit then noSit:Disconnect() nositDied:Disconnect() end
  end)
  
  addcmd("jump", {}, function(args, speaker)
    speaker.Character:FindFirstChildWhichIsA("Humanoid"):ChangeState(Enum.HumanoidStateType.Jumping)
  end)
  
  local infJump
  infJumpDebounce = false
  addcmd("infjump", {"infinitejump"}, function(args, speaker)
    if infJump then infJump:Disconnect() end
    infJumpDebounce = false
    infJump = UserInputService.JumpRequest:Connect(function()
      if not infJumpDebounce then
        infJumpDebounce = true
        speaker.Character:FindFirstChildWhichIsA("Humanoid"):ChangeState(Enum.HumanoidStateType.Jumping)
        wait()
        infJumpDebounce = false
      end
    end)
  end)
  
  addcmd("uninfjump", {"uninfinitejump", "noinfjump", "noinfinitejump"}, function(args, speaker)
    if infJump then infJump:Disconnect() end
    infJumpDebounce = false
  end)
  
  local flyjump
  addcmd("flyjump", {}, function(args, speaker)
    if flyjump then flyjump:Disconnect() end
    flyjump = UserInputService.JumpRequest:Connect(function()
      speaker.Character:FindFirstChildWhichIsA("Humanoid"):ChangeState(Enum.HumanoidStateType.Jumping)
    end)
  end)
  
  addcmd("unflyjump", {"noflyjump"}, function(args, speaker)
    if flyjump then flyjump:Disconnect() end
  end)
  
  local HumanModCons = {}
  addcmd('autojump',{'ajump'},function(args, speaker)
    local Char = speaker.Character
    local Human = Char and Char:FindFirstChildWhichIsA("Humanoid")
    local function autoJump()
      if Char and Human then
        local check1 = workspace:FindPartOnRay(Ray.new(Human.RootPart.Position-Vector3.new(0,1.5,0), Human.RootPart.CFrame.lookVector*3), Human.Parent)
        local check2 = workspace:FindPartOnRay(Ray.new(Human.RootPart.Position+Vector3.new(0,1.5,0), Human.RootPart.CFrame.lookVector*3), Human.Parent)
        if check1 or check2 then
          Human.Jump = true
        end
      end
    end
    autoJump()
    HumanModCons.ajLoop = (HumanModCons.ajLoop and HumanModCons.ajLoop:Disconnect() and false) or RunService.RenderStepped:Connect(autoJump)
    HumanModCons.ajCA = (HumanModCons.ajCA and HumanModCons.ajCA:Disconnect() and false) or speaker.CharacterAdded:Connect(function(nChar)
      Char, Human = nChar, nChar:WaitForChild("Humanoid")
      autoJump()
      HumanModCons.ajLoop = (HumanModCons.ajLoop and HumanModCons.ajLoop:Disconnect() and false) or RunService.RenderStepped:Connect(autoJump)
    end)
  end)
  
  addcmd('unautojump',{'noautojump', 'noajump', 'unajump'},function(args, speaker)
    HumanModCons.ajLoop = (HumanModCons.ajLoop and HumanModCons.ajLoop:Disconnect() and false) or nil
    HumanModCons.ajCA = (HumanModCons.ajCA and HumanModCons.ajCA:Disconnect() and false) or nil
  end)
  
  addcmd('edgejump',{'ejump'},function(args, speaker)
    local Char = speaker.Character
    local Human = Char and Char:FindFirstChildWhichIsA("Humanoid")
    -- Full credit to NoelGamer06 @V3rmillion
    local state
    local laststate
    local lastcf
    local function edgejump()
      if Char and Human then
        laststate = state
        state = Human:GetState()
        if laststate ~= state and state == Enum.HumanoidStateType.Freefall and laststate ~= Enum.HumanoidStateType.Jumping then
          Char.HumanoidRootPart.CFrame = lastcf
          Char.HumanoidRootPart.Velocity = Vector3.new(Char.HumanoidRootPart.Velocity.X, Human.JumpPower or Human.JumpHeight, Char.HumanoidRootPart.Velocity.Z)
        end
        lastcf = Char.HumanoidRootPart.CFrame
      end
    end
    edgejump()
    HumanModCons.ejLoop = (HumanModCons.ejLoop and HumanModCons.ejLoop:Disconnect() and false) or RunService.RenderStepped:Connect(edgejump)
    HumanModCons.ejCA = (HumanModCons.ejCA and HumanModCons.ejCA:Disconnect() and false) or speaker.CharacterAdded:Connect(function(nChar)
      Char, Human = nChar, nChar:WaitForChild("Humanoid")
      edgejump()
      HumanModCons.ejLoop = (HumanModCons.ejLoop and HumanModCons.ejLoop:Disconnect() and false) or RunService.RenderStepped:Connect(edgejump)
    end)
  end)
  
  addcmd('unedgejump',{'noedgejump', 'noejump', 'unejump'},function(args, speaker)
    HumanModCons.ejLoop = (HumanModCons.ejLoop and HumanModCons.ejLoop:Disconnect() and false) or nil
    HumanModCons.ejCA = (HumanModCons.ejCA and HumanModCons.ejCA:Disconnect() and false) or nil
  end)
  
  addcmd('team',{},function(args, speaker)
    local teamname = nil
    for a,b in pairs(Teams:GetChildren()) do
      local L_name = b.Name:lower()
      local F = L_name:find(getstring(1))
      if F == 1 then
        teamname = b 
      end
    end
    speaker.Team = teamname
  end)
  
  addcmd('nobgui',{'unbgui','nobillboardgui','unbillboardgui','noname','rohg'},function(args, speaker)
    for i,v in pairs(speaker.Character:GetDescendants())do
      if v:IsA("BillboardGui") or v:IsA("SurfaceGui") then
        v:Destroy()
      end
    end
  end)
  
  addcmd('loopnobgui',{'loopunbgui','loopnobillboardgui','loopunbillboardgui','loopnoname','looprohg'},function(args, speaker)
    for i,v in pairs(speaker.Character:GetDescendants())do
      if v:IsA("BillboardGui") or v:IsA("SurfaceGui") then
        v:Destroy()
      end
    end
    local function charPartAdded(part)
      if part:IsA("BillboardGui") or part:IsA("SurfaceGui") then
        wait()
        part:Destroy()
      end
    end
    charPartTrigger = speaker.Character.DescendantAdded:Connect(charPartAdded)
  end)
  
  addcmd('unloopnobgui',{'unloopunbgui','unloopnobillboardgui','unloopunbillboardgui','unloopnoname','unlooprohg'},function(args, speaker)
    if charPartTrigger then
      charPartTrigger:Disconnect()
    end
  end)
  
  addcmd('spasm',{},function(args, speaker)
    if not r15(speaker) then
      local pchar=speaker.Character
      local AnimationId = "33796059"
      SpasmAnim = Instance.new("Animation")
      SpasmAnim.AnimationId = "rbxassetid://"..AnimationId
      Spasm = pchar:FindFirstChildOfClass('Humanoid'):LoadAnimation(SpasmAnim)
      Spasm:Play()
      Spasm:AdjustSpeed(99)
    else
      notify('R6 Required','This command requires the r6 rig type')
    end
  end)
  
  addcmd('unspasm',{'nospasm'},function(args, speaker)
    Spasm:Stop()
    SpasmAnim:Destroy()
  end)
  
  addcmd('headthrow',{},function(args, speaker)
    if not r15(speaker) then
      local AnimationId = "35154961"
      local Anim = Instance.new("Animation")
      Anim.AnimationId = "rbxassetid://"..AnimationId
      local k = speaker.Character:FindFirstChildOfClass('Humanoid'):LoadAnimation(Anim)
      k:Play(0)
      k:AdjustSpeed(1)
    else
      notify('R6 Required','This command requires the r6 rig type')
    end
  end)
  
  addcmd('animation',{'anim'},function(args, speaker)
    if not r15(speaker) then
      local pchar=speaker.Character
      local AnimationId = tostring(args[1])
      local Anim = Instance.new("Animation")
      Anim.AnimationId = "rbxassetid://"..AnimationId
      local k = pchar:FindFirstChildOfClass('Humanoid'):LoadAnimation(Anim)
      k:Play()
      if args[2] then
        k:AdjustSpeed(tostring(args[2]))
      end
    else
      notify('R6 Required','This command requires the r6 rig type')
    end
  end)
  
  addcmd('noanim',{},function(args, speaker)
    speaker.Character.Animate.Disabled = true
  end)
  
  addcmd('reanim',{},function(args, speaker)
    speaker.Character.Animate.Disabled = false
  end)
  
  addcmd('animspeed',{},function(args, speaker)
    local Char = speaker.Character
    local Hum = Char:FindFirstChildOfClass("Humanoid") or Char:FindFirstChildOfClass("AnimationController")
  
    for i,v in next, Hum:GetPlayingAnimationTracks() do
      v:AdjustSpeed(tonumber(args[1] or 1))
    end
  end)
  
  addcmd('copyanimation',{'copyanim','copyemote'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for _,v in ipairs(players)do
      local char = Players[v].Character
      for _, v1 in pairs(speaker.Character:FindFirstChildOfClass('Humanoid'):GetPlayingAnimationTracks()) do
        v1:Stop()
      end
      for _, v1 in pairs(Players[v].Character:FindFirstChildOfClass('Humanoid'):GetPlayingAnimationTracks()) do
        if not string.find(v1.Animation.AnimationId, "507768375") then
          local ANIM = speaker.Character:FindFirstChildOfClass('Humanoid'):LoadAnimation(v1.Animation)
          ANIM:Play(.1, 1, v1.Speed)
          ANIM.TimePosition = v1.TimePosition
          task.spawn(function()
            v1.Stopped:Wait()
            ANIM:Stop()
            ANIM:Destroy()
          end)
        end
      end
    end
  end)
  
  addcmd('stopanimations',{'stopanims','stopanim'},function(args, speaker)
    local Char = speaker.Character
    local Hum = Char:FindFirstChildOfClass("Humanoid") or Char:FindFirstChildOfClass("AnimationController")
  
    for i,v in next, Hum:GetPlayingAnimationTracks() do
      v:Stop()
    end
  end)
  
  addcmd('refreshanimations', {'refreshanimation', 'refreshanims', 'refreshanim'}, function(args, speaker)
    local Char = speaker.Character or speaker.CharacterAdded:Wait()
    local Human = Char and Char:WaitForChild('Humanoid', 15)
    local Animate = Char and Char:WaitForChild('Animate', 15)
    if not Human or not Animate then
      return notify('Refresh Animations', 'Failed to get Animate/Humanoid')
    end
    Animate.Disabled = true
    for _, v in ipairs(Human:GetPlayingAnimationTracks()) do
      v:Stop()
    end
    Animate.Disabled = false
  end)
  
  addcmd('allowcustomanim', {'allowcustomanimations'}, function(args, speaker)
    StarterPlayer.AllowCustomAnimations = true
    execCmd('refreshanimations')
  end)
  
  addcmd('unallowcustomanim', {'unallowcustomanimations'}, function(args, speaker)
    StarterPlayer.AllowCustomAnimations = false
    execCmd('refreshanimations')
  end)
  
  addcmd('loopanimation', {'loopanim'},function(args, speaker)
    local Char = speaker.Character
    local Human = Char and Char.FindFirstChildWhichIsA(Char, "Humanoid")
    for _, v in ipairs(Human.GetPlayingAnimationTracks(Human)) do
      v.Looped = true
    end
  end)
  
  addcmd('tpposition',{'tppos'},function(args, speaker)
    if #args < 3 then return end
    local tpX,tpY,tpZ = tonumber(args[1]),tonumber(args[2]),tonumber(args[3])
    local char = speaker.Character
    if char and getRoot(char) then
      getRoot(char).CFrame = CFrame.new(tpX,tpY,tpZ)
    end
  end)
  
  addcmd('tweentpposition',{'ttppos'},function(args, speaker)
    if #args < 3 then return end
    local tpX,tpY,tpZ = tonumber(args[1]),tonumber(args[2]),tonumber(args[3])
    local char = speaker.Character
    if char and getRoot(char) then
      TweenService:Create(getRoot(speaker.Character), TweenInfo.new(tweenSpeed, Enum.EasingStyle.Linear), {CFrame = CFrame.new(tpX,tpY,tpZ)}):Play()
    end
  end)
  
  addcmd('offset',{},function(args, speaker)
    if #args < 3 then
      return 
    end
    if speaker.Character then
      speaker.Character:TranslateBy(Vector3.new(tonumber(args[1]) or 0, tonumber(args[2]) or 0, tonumber(args[3]) or 0))
    end
  end)
  
  addcmd('tweenoffset',{'toffset'},function(args, speaker)
    if #args < 3 then return end
    local tpX,tpY,tpZ = tonumber(args[1]),tonumber(args[2]),tonumber(args[3])
    local char = speaker.Character
    if char and getRoot(char) then
      TweenService:Create(getRoot(speaker.Character), TweenInfo.new(tweenSpeed, Enum.EasingStyle.Linear), {CFrame = CFrame.new(tpX,tpY,tpZ)}):Play()
    end
  end)
  
  addcmd('clickteleport',{},function(args, speaker)
    if speaker == Players.LocalPlayer then
      notify('Click TP','Go to Settings>Keybinds>Add to set up click tp')
    end
  end)
  
  addcmd('tptool', {'teleporttool'}, function(args, speaker)
    local TpTool = Instance.new("Tool")
    TpTool.Name = "Teleport Tool"
    TpTool.RequiresHandle = false
    TpTool.Parent = speaker.Backpack
    TpTool.Activated:Connect(function()
      local Char = speaker.Character or workspace:FindFirstChild(speaker.Name)
      local HRP = Char and Char:FindFirstChild("HumanoidRootPart")
      if not Char or not HRP then
        return warn("Failed to find HumanoidRootPart")
      end
      HRP.CFrame = CFrame.new(IYMouse.Hit.X, IYMouse.Hit.Y + 3, IYMouse.Hit.Z, select(4, HRP.CFrame:components()))
    end)
  end)
  
  addcmd('clickdelete',{},function(args, speaker)
    if speaker == Players.LocalPlayer then
      notify('Click Delete','Go to Settings>Keybinds>Add to set up click delete')
    end
  end)
  
  addcmd('getposition',{'getpos','notifypos','notifyposition'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      local char = Players[v].Character
      local pos = char and (getRoot(char) or char:FindFirstChildWhichIsA("BasePart"))
      pos = pos and pos.Position
      if not pos then
        return notify('Getposition Error','Missing character')
      end
      local roundedPos = math.round(pos.X) .. ", " .. math.round(pos.Y) .. ", " .. math.round(pos.Z)
      notify('Current Position',roundedPos)
    end
  end)
  
  addcmd('copyposition',{'copypos'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      local char = Players[v].Character
      local pos = char and (getRoot(char) or char:FindFirstChildWhichIsA("BasePart"))
      pos = pos and pos.Position
      if not pos then
        return notify('Getposition Error','Missing character')
      end
      local roundedPos = math.round(pos.X) .. ", " .. math.round(pos.Y) .. ", " .. math.round(pos.Z)
      toClipboard(roundedPos)
    end
  end)
  
  addcmd('walktopos',{'walktoposition'},function(args, speaker)
    if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
      speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
      wait(.1)
    end
    speaker.Character:FindFirstChildOfClass('Humanoid').WalkToPoint = Vector3.new(args[1],args[2],args[3])
  end)
  
  addcmd('speed',{'ws','walkspeed'},function(args, speaker)
    if args[2] then
      local speed = args[2] or 16
      if isNumber(speed) then
        speaker.Character:FindFirstChildOfClass('Humanoid').WalkSpeed = speed
      end
    else
      local speed = args[1] or 16
      if isNumber(speed) then
        speaker.Character:FindFirstChildOfClass('Humanoid').WalkSpeed = speed
      end
    end
  end)
  
  addcmd('spoofspeed',{'spoofws','spoofwalkspeed'},function(args, speaker)
    if args[1] and isNumber(args[1]) then
      if hookmetamethod then
        local char = speaker.Character
        local setspeed;
        local index; index = hookmetamethod(game, "__index", function(self, key)
          local keyclean = key:gsub("\0", "")
          if (keyclean == "WalkSpeed" or keyclean == "walkSpeed") and self:IsA("Humanoid") and self:IsDescendantOf(char) and not checkcaller() then
            return setspeed or args[1]
          end
          return index(self, key)
        end)
        local newindex; newindex = hookmetamethod(game, "__newindex", function(self, key, value)
          local keyclean = string.gsub(key, "\0", "")
          if (keyclean == "WalkSpeed" or keyclean == "walkSpeed") and self:IsA("Humanoid") and self:IsDescendantOf(char) and not checkcaller() then
            setspeed = tonumber(value)
            return setspeed
          end
          return newindex(self, key, value)
        end)
      else
        notify('Incompatible Exploit','Your exploit does not support this command (missing hookmetamethod)')
      end
    end
  end)
  
  addcmd('loopspeed',{'loopws'},function(args, speaker)
    local speed = args[1] or 16
    if args[2] then
      speed = args[2] or 16
    end
    if isNumber(speed) then
      local Char = speaker.Character or workspace:FindFirstChild(speaker.Name)
      local Human = Char and Char:FindFirstChildWhichIsA("Humanoid")
      local function WalkSpeedChange()
        if Char and Human then
          Human.WalkSpeed = speed
        end
      end
      WalkSpeedChange()
      HumanModCons.wsLoop = (HumanModCons.wsLoop and HumanModCons.wsLoop:Disconnect() and false) or Human:GetPropertyChangedSignal("WalkSpeed"):Connect(WalkSpeedChange)
      HumanModCons.wsCA = (HumanModCons.wsCA and HumanModCons.wsCA:Disconnect() and false) or speaker.CharacterAdded:Connect(function(nChar)
        Char, Human = nChar, nChar:WaitForChild("Humanoid")
        WalkSpeedChange()
        HumanModCons.wsLoop = (HumanModCons.wsLoop and HumanModCons.wsLoop:Disconnect() and false) or Human:GetPropertyChangedSignal("WalkSpeed"):Connect(WalkSpeedChange)
      end)
    end
  end)
  
  addcmd('unloopspeed',{'unloopws'},function(args, speaker)
    HumanModCons.wsLoop = (HumanModCons.wsLoop and HumanModCons.wsLoop:Disconnect() and false) or nil
    HumanModCons.wsCA = (HumanModCons.wsCA and HumanModCons.wsCA:Disconnect() and false) or nil
  end)
  
  addcmd('spoofjumppower',{'spoofjp'},function(args, speaker)
    if args[1] and isNumber(args[1]) then
      if hookmetamethod then
        local char = speaker.Character
        local setpower;
        local index; index = hookmetamethod(game, "__index", function(self, key)
          local keyclean = key:gsub("\0", "")
          if (keyclean == "JumpPower" or keyclean == "jumpPower") and self:IsA("Humanoid") and self:IsDescendantOf(char) and not checkcaller() then
            return setpower or args[1]
          end
          return index(self, key)
        end)
        local newindex; newindex = hookmetamethod(game, "__newindex", function(self, key, value)
          local keyclean = string.gsub(key, "\0", "")
          if (keyclean == "JumpPower" or keyclean == "jumpPower") and self:IsA("Humanoid") and self:IsDescendantOf(char) and not checkcaller() then
            setpower = tonumber(value)
            return setpower
          end
          return newindex(self, key, value)
        end)
      else
        notify('Incompatible Exploit','Your exploit does not support this command (missing hookmetamethod)')
      end
    end
  end)
  
  addcmd('loopjumppower',{'loopjp','loopjpower'},function(args, speaker)
    local jpower = args[1] or 50
    if isNumber(jpower) then
      local Char = speaker.Character or workspace:FindFirstChild(speaker.Name)
      local Human = Char and Char:FindFirstChildWhichIsA("Humanoid")
      local function JumpPowerChange()
        if Char and Human then
          if speaker.Character:FindFirstChildOfClass('Humanoid').UseJumpPower then
            speaker.Character:FindFirstChildOfClass('Humanoid').JumpPower = jpower
          else
            speaker.Character:FindFirstChildOfClass('Humanoid').JumpHeight  = jpower
          end
        end
      end
      JumpPowerChange()
      HumanModCons.jpLoop = (HumanModCons.jpLoop and HumanModCons.jpLoop:Disconnect() and false) or Human:GetPropertyChangedSignal("JumpPower"):Connect(JumpPowerChange)
      HumanModCons.jpCA = (HumanModCons.jpCA and HumanModCons.jpCA:Disconnect() and false) or speaker.CharacterAdded:Connect(function(nChar)
        Char, Human = nChar, nChar:WaitForChild("Humanoid")
        JumpPowerChange()
        HumanModCons.jpLoop = (HumanModCons.jpLoop and HumanModCons.jpLoop:Disconnect() and false) or Human:GetPropertyChangedSignal("JumpPower"):Connect(JumpPowerChange)
      end)
    end
  end)
  
  addcmd('unloopjumppower',{'unloopjp','unloopjpower'},function(args, speaker)
    local Char = speaker.Character or workspace:FindFirstChild(speaker.Name)
    local Human = Char and Char:FindFirstChildWhichIsA("Humanoid")
    HumanModCons.jpLoop = (HumanModCons.jpLoop and HumanModCons.jpLoop:Disconnect() and false) or nil
    HumanModCons.jpCA = (HumanModCons.jpCA and HumanModCons.jpCA:Disconnect() and false) or nil
    if Char and Human then
      if speaker.Character:FindFirstChildOfClass('Humanoid').UseJumpPower then
        speaker.Character:FindFirstChildOfClass('Humanoid').JumpPower = 50
      else
        speaker.Character:FindFirstChildOfClass('Humanoid').JumpHeight  = 50
      end
    end
  end)
  
  addcmd('tools',{'gears'},function(args, speaker)
    local function copy(instance)
      for i,c in pairs(instance:GetChildren())do
        if c:IsA('Tool') or c:IsA('HopperBin') then
          c:Clone().Parent = speaker:FindFirstChildOfClass("Backpack")
        end
        copy(c)
      end
    end
    copy(Lighting)
    local function copy(instance)
      for i,c in pairs(instance:GetChildren())do
        if c:IsA('Tool') or c:IsA('HopperBin') then
          c:Clone().Parent = speaker:FindFirstChildOfClass("Backpack")
        end
        copy(c)
      end
    end
    copy(ReplicatedStorage)
    notify('Tools','Copied tools from ReplicatedStorage and Lighting')
  end)
  
  addcmd('notools',{'rtools','clrtools','removetools','deletetools','dtools'},function(args, speaker)
    for i,v in pairs(speaker:FindFirstChildOfClass("Backpack"):GetDescendants()) do
      if v:IsA('Tool') or v:IsA('HopperBin') then
        v:Destroy()
      end
    end
    for i,v in pairs(speaker.Character:GetDescendants()) do
      if v:IsA('Tool') or v:IsA('HopperBin') then
        v:Destroy()
      end
    end
  end)
  
  addcmd('deleteselectedtool',{'dst'},function(args, speaker)
    for i,v in pairs(speaker.Character:GetDescendants()) do
      if v:IsA('Tool') or v:IsA('HopperBin') then
        v:Destroy()
      end
    end
  end)
  
  addcmd('console',{},function(args, speaker)
    -- Thanks wally!!
    notify("Loading",'Hold on a sec')
    local _, str = pcall(function()
      return game:HttpGet("https://raw.githubusercontent.com/infyiff/backup/main/console.lua", true)
    end)
  
    local s, e = loadstring(str)
    if typeof(s) ~= "function" then
      return
    end
  
    local success, message = pcall(s)
    if (not success) then
      if printconsole then
        printconsole(message)
      elseif printoutput then
        printoutput(message)
      end
    end
    wait(1)
    notify('Console','Press F9 to open the console')
  end)
  
  addcmd('explorer', {'dex'}, function(args, speaker)
    notify('Loading', 'Hold on a sec')
    loadstring(game:HttpGet("https://raw.githubusercontent.com/infyiff/backup/main/dex.lua"))()
  end)
  
  addcmd('olddex', {'odex'}, function(args, speaker)
    notify('Loading old explorer', 'Hold on a sec')
  
    local getobjects = function(a)
      local Objects = {}
      if a then
        local b = InsertService:LoadLocalAsset(a)
        if b then 
          table.insert(Objects, b) 
        end
      end
      return Objects
    end
  
    local Dex = getobjects("rbxassetid://10055842438")[1]
    Dex.Parent = PARENT
  
    local function Load(Obj, Url)
      local function GiveOwnGlobals(Func, Script)
        -- Fix for this edit of dex being poorly made
        -- I (Alex) would like to commemorate whoever added this dex in somehow finding the worst dex to ever exist
        local Fenv, RealFenv, FenvMt = {}, {
          script = Script,
          getupvalue = function(a, b)
            return nil -- force it to use globals
          end,
          getreg = function() -- It loops registry for some idiotic reason so stop it from doing that and just use a global
            return {} -- force it to use globals
          end,
          getprops = getprops or function(inst)
            if getproperties then
              local props = getproperties(inst)
              if props[1] and gethiddenproperty then
                local results = {}
                for _,name in pairs(props) do
                  local success, res = pcall(gethiddenproperty, inst, name)
                  if success then
                    results[name] = res
                  end
                end
  
                return results
              end
  
              return props
            end
  
            return {}
          end
        }, {}
        FenvMt.__index = function(a,b)
          return RealFenv[b] == nil and getgenv()[b] or RealFenv[b]
        end
        FenvMt.__newindex = function(a, b, c)
          if RealFenv[b] == nil then 
            getgenv()[b] = c 
          else 
            RealFenv[b] = c 
          end
        end
        setmetatable(Fenv, FenvMt)
        pcall(setfenv, Func, Fenv)
        return Func
      end
  
      local function LoadScripts(_, Script)
        if Script:IsA("LocalScript") then
          task.spawn(function()
            GiveOwnGlobals(loadstring(Script.Source,"="..Script:GetFullName()), Script)()
          end)
        end
        table.foreach(Script:GetChildren(), LoadScripts)
      end
  
      LoadScripts(nil, Obj)
    end
  
    Load(Dex)
  end)
  
  addcmd('remotespy',{'rspy'},function(args, speaker)
    notify("Loading",'Hold on a sec')
    -- Full credit to exx, creator of SimpleSpy
    -- also thanks to NoobSploit for fixing
    loadstring(game:HttpGet("https://raw.githubusercontent.com/infyiff/backup/main/SimpleSpyV3/main.lua"))()
  end)
  
  addcmd('audiologger',{'alogger'},function(args, speaker)
    notify("Loading",'Hold on a sec')
    loadstring(game:HttpGet(('https://raw.githubusercontent.com/infyiff/backup/main/audiologger.lua'),true))()
  end)
  
  local loopgoto = nil
  addcmd('loopgoto',{},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      loopgoto = nil
      if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
        speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
        wait(.1)
      end
      loopgoto = Players[v]
      local distance = 3
      if args[2] and isNumber(args[2]) then
        distance = args[2]
      end
      local lDelay = 0
      if args[3] and isNumber(args[3]) then
        lDelay = args[3]
      end
      repeat
        if Players:FindFirstChild(v) then
          if Players[v].Character ~= nil then
            getRoot(speaker.Character).CFrame = getRoot(Players[v].Character).CFrame + Vector3.new(distance,1,0)
          end
          wait(lDelay)
        else
          loopgoto = nil
        end
      until loopgoto ~= Players[v]
    end
  end)
  
  addcmd('unloopgoto',{'noloopgoto'},function(args, speaker)
    loopgoto = nil
  end)
  
  addcmd('headsit',{},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    if headSit then headSit:Disconnect() end
    for i,v in pairs(players)do
      speaker.Character:FindFirstChildOfClass('Humanoid').Sit = true
      headSit = RunService.Heartbeat:Connect(function()
        if Players:FindFirstChild(Players[v].Name) and Players[v].Character ~= nil and getRoot(Players[v].Character) and getRoot(speaker.Character) and speaker.Character:FindFirstChildOfClass('Humanoid').Sit == true then
          getRoot(speaker.Character).CFrame = getRoot(Players[v].Character).CFrame * CFrame.Angles(0,math.rad(0),0)* CFrame.new(0,1.6,0.4)
        else
          headSit:Disconnect()
        end
      end)
    end
  end)
  
  addcmd('chat',{'say'},function(args, speaker)
    local cString = getstring(1)
    chatMessage(cString)
  end)
  
  
  spamming = false
  spamspeed = 1
  addcmd('spam',{},function(args, speaker)
    spamming = true
    local spamstring = getstring(1)
    repeat wait(spamspeed)
      chatMessage(spamstring)
    until spamming == false
  end)
  
  addcmd('nospam',{'unspam'},function(args, speaker)
    spamming = false
  end)
  
  addcmd('whisper',{'pm'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      task.spawn(function()
        local plrName = Players[v].Name
        local pmstring = getstring(2)
        chatMessage("/w "..plrName.." "..pmstring)
      end)
    end
  end)
  
  pmspamming = {}
  addcmd('pmspam',{},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      task.spawn(function()
        local plrName = Players[v].Name
        if FindInTable(pmspamming, plrName) then return end
        table.insert(pmspamming, plrName)
        local pmspamstring = getstring(2)
        repeat
          if Players:FindFirstChild(v) then
            wait(spamspeed)
            chatMessage("/w "..plrName.." "..pmspamstring)
          else
            for a,b in pairs(pmspamming) do if b == plrName then table.remove(pmspamming, a) end end
          end
        until not FindInTable(pmspamming, plrName)
      end)
    end
  end)
  
  addcmd('nopmspam',{'unpmspam'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      task.spawn(function()
        for a,b in pairs(pmspamming) do
          if b == Players[v].Name then
            table.remove(pmspamming, a)
          end
        end
      end)
    end
  end)
  
  addcmd('spamspeed',{},function(args, speaker)
    local speed = args[1] or 1
    if isNumber(speed) then
      spamspeed = speed
    end
  end)
  
  addcmd('bubblechat',{},function(args, speaker)
    ChatService.BubbleChatEnabled = true
  end)
  
  addcmd('unbubblechat',{'nobubblechat'},function(args, speaker)
    ChatService.BubbleChatEnabled = false
  end)
  
  addcmd('safechat',{},function(args, speaker)
    speaker:SetSuperSafeChat(true)
  end)
  
  addcmd('nosafechat',{'disablesafechat','unsafechat'},function(args, speaker)
    speaker:SetSuperSafeChat(false)
  end)
  
  addcmd('blockhead',{},function(args, speaker)
    speaker.Character.Head:FindFirstChildOfClass("SpecialMesh"):Destroy()
  end)
  
  addcmd('blockhats',{},function(args, speaker)
    for _,v in pairs(speaker.Character:FindFirstChildOfClass('Humanoid'):GetAccessories()) do
      for i,c in pairs(v:GetDescendants()) do
        if c:IsA("SpecialMesh") then
          c:Destroy()
        end
      end
    end
  end)
  
  addcmd('blocktool',{},function(args, speaker)
    for _,v in pairs(speaker.Character:GetChildren()) do
      if v:IsA("Tool") or v:IsA("HopperBin") then
        for i,c in pairs(v:GetDescendants()) do
          if c:IsA("SpecialMesh") then
            c:Destroy()
          end
        end
      end
    end
  end)
  
  addcmd('creeper',{},function(args, speaker)
    if r15(speaker) then
      speaker.Character.Head:FindFirstChildOfClass("SpecialMesh"):Destroy()
      speaker.Character.LeftUpperArm:Destroy()
      speaker.Character.RightUpperArm:Destroy()
      speaker.Character:FindFirstChildOfClass("Humanoid"):RemoveAccessories()
    else
      speaker.Character.Head:FindFirstChildOfClass("SpecialMesh"):Destroy()
      speaker.Character["Left Arm"]:Destroy()
      speaker.Character["Right Arm"]:Destroy()
      speaker.Character:FindFirstChildOfClass("Humanoid"):RemoveAccessories()
    end
  end)
  
  function getTorso(x)
    x = x or Players.LocalPlayer.Character
    return x:FindFirstChild("Torso") or x:FindFirstChild("UpperTorso") or x:FindFirstChild("LowerTorso") or x:FindFirstChild("HumanoidRootPart")
  end
  
  addcmd("bang", {"rape"}, function(args, speaker)
    execCmd("unbang")
    wait()
    local humanoid = speaker.Character:FindFirstChildWhichIsA("Humanoid")
    bangAnim = Instance.new("Animation")
    bangAnim.AnimationId = not r15(speaker) and "rbxassetid://148840371" or "rbxassetid://5918726674"
    bang = humanoid:LoadAnimation(bangAnim)
    bang:Play(0.1, 1, 1)
    bang:AdjustSpeed(args[2] or 3)
    bangDied = humanoid.Died:Connect(function()
      bang:Stop()
      bangAnim:Destroy()
      bangDied:Disconnect()
      bangLoop:Disconnect()
    end)
    if args[1] then
      local players = getPlayer(args[1], speaker)
      for _, v in pairs(players) do
        local bangplr = Players[v].Name
        local bangOffet = CFrame.new(0, 0, 1.1)
        bangLoop = RunService.Stepped:Connect(function()
          pcall(function()
            local otherRoot = getTorso(Players[bangplr].Character)
            getRoot(speaker.Character).CFrame = otherRoot.CFrame * bangOffet
          end)
        end)
      end
    end
  end)
  
  addcmd("unbang", {"unrape"}, function(args, speaker)
    if bangDied then
      bangDied:Disconnect()
      bang:Stop()
      bangAnim:Destroy()
      bangLoop:Disconnect()
    end
  end)
  
  addcmd('carpet',{},function(args, speaker)
    if not r15(speaker) then
      execCmd('uncarpet')
      wait()
      local players = getPlayer(args[1], speaker)
      for i,v in pairs(players)do
        carpetAnim = Instance.new("Animation")
        carpetAnim.AnimationId = "rbxassetid://282574440"
        carpet = speaker.Character:FindFirstChildOfClass('Humanoid'):LoadAnimation(carpetAnim)
        carpet:Play(.1, 1, 1)
        local carpetplr = Players[v].Name
        carpetDied = speaker.Character:FindFirstChildOfClass'Humanoid'.Died:Connect(function()
          carpetLoop:Disconnect()
          carpet:Stop()
          carpetAnim:Destroy()
          carpetDied:Disconnect()
        end)
        carpetLoop = RunService.Heartbeat:Connect(function()
          pcall(function()
            getRoot(Players.LocalPlayer.Character).CFrame = getRoot(Players[carpetplr].Character).CFrame
          end)
        end)
      end
    else
      notify('R6 Required','This command requires the r6 rig type')
    end
  end)
  
  addcmd('uncarpet',{'nocarpet'},function(args, speaker)
    if carpetLoop then
      carpetLoop:Disconnect()
      carpetDied:Disconnect()
      carpet:Stop()
      carpetAnim:Destroy()
    end
  end)
  
  addcmd('friend',{},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      speaker:RequestFriendship(v)
    end
  end)
  
  addcmd('unfriend',{},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      speaker:RevokeFriendship(v)
    end
  end)
  
  addcmd('bringpart',{},function(args, speaker)
    for i,v in pairs(workspace:GetDescendants()) do
      if v.Name:lower() == getstring(1):lower() and v:IsA("BasePart") then
        v.CFrame = getRoot(speaker.Character).CFrame
      end
    end
  end)
  
  addcmd('bringpartclass',{'bpc'},function(args, speaker)
    for i,v in pairs(workspace:GetDescendants()) do
      if v.ClassName:lower() == getstring(1):lower() and v:IsA("BasePart") then
        v.CFrame = getRoot(speaker.Character).CFrame
      end
    end
  end)
  
  gotopartDelay = 0.1
  addcmd('gotopart',{'topart'},function(args, speaker)
    for i,v in pairs(workspace:GetDescendants()) do
      if v.Name:lower() == getstring(1):lower() and v:IsA("BasePart") then
        if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
          speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
          wait(.1)
        end
        wait(gotopartDelay)
        getRoot(speaker.Character).CFrame = v.CFrame
      end
    end
  end)
  
  addcmd('tweengotopart',{'tgotopart','ttopart'},function(args, speaker)
    for i,v in pairs(workspace:GetDescendants()) do
      if v.Name:lower() == getstring(1):lower() and v:IsA("BasePart") then
        if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
          speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
          wait(.1)
        end
        wait(gotopartDelay)
        TweenService:Create(getRoot(speaker.Character), TweenInfo.new(tweenSpeed, Enum.EasingStyle.Linear), {CFrame = v.CFrame}):Play()
      end
    end
  end)
  
  addcmd('gotopartclass',{'gpc'},function(args, speaker)
    for i,v in pairs(workspace:GetDescendants()) do
      if v.ClassName:lower() == getstring(1):lower() and v:IsA("BasePart") then
        if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
          speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
          wait(.1)
        end
        wait(gotopartDelay)
        getRoot(speaker.Character).CFrame = v.CFrame
      end
    end
  end)
  
  addcmd('tweengotopartclass',{'tgpc'},function(args, speaker)
    for i,v in pairs(workspace:GetDescendants()) do
      if v.ClassName:lower() == getstring(1):lower() and v:IsA("BasePart") then
        if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
          speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
          wait(.1)
        end
        wait(gotopartDelay)
        TweenService:Create(getRoot(speaker.Character), TweenInfo.new(tweenSpeed, Enum.EasingStyle.Linear), {CFrame = v.CFrame}):Play()
      end
    end
  end)
  
  addcmd('gotomodel',{'tomodel'},function(args, speaker)
    for i,v in pairs(workspace:GetDescendants()) do
      if v.Name:lower() == getstring(1):lower() and v:IsA("Model") then
        if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
          speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
          wait(.1)
        end
        wait(gotopartDelay)
        getRoot(speaker.Character).CFrame = v:GetModelCFrame()
      end
    end
  end)
  
  addcmd('tweengotomodel',{'tgotomodel','ttomodel'},function(args, speaker)
    for i,v in pairs(workspace:GetDescendants()) do
      if v.Name:lower() == getstring(1):lower() and v:IsA("Model") then
        if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
          speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
          wait(.1)
        end
        wait(gotopartDelay)
        TweenService:Create(getRoot(speaker.Character), TweenInfo.new(tweenSpeed, Enum.EasingStyle.Linear), {CFrame = v:GetModelCFrame()}):Play()
      end
    end
  end)
  
  addcmd('gotopartdelay',{},function(args, speaker)
    local gtpDelay = args[1] or 0.1
    if isNumber(gtpDelay) then
      gotopartDelay = gtpDelay
    end
  end)
  
  addcmd('noclickdetectorlimits',{'nocdlimits','removecdlimits'},function(args, speaker)
    for i,v in ipairs(workspace:GetDescendants()) do
      if v:IsA("ClickDetector") then
        v.MaxActivationDistance = math.huge
      end
    end
  end)
  
  addcmd('fireclickdetectors',{'firecd','firecds'}, function(args, speaker)
    if fireclickdetector then
      if args[1] then
        local name = getstring(1)
        for _, descendant in ipairs(workspace:GetDescendants()) do
          if descendant:IsA("ClickDetector") and descendant.Name == name or descandant.Parent.Name == name then
            fireclickdetector(descendant)
          end
        end
      else
        for _, descendant in ipairs(workspace:GetDescendants()) do
          if descendant:IsA("ClickDetector") then
            fireclickdetector(descendant)
          end
        end
      end
    else
      notify("Incompatible Exploit", "Your exploit does not support this command (missing fireclickdetector)")
    end
  end)
  
  addcmd('noproximitypromptlimits',{'nopplimits','removepplimits'},function(args, speaker)
    for i,v in pairs(workspace:GetDescendants()) do
      if v:IsA("ProximityPrompt") then
        v.MaxActivationDistance = math.huge
      end
    end
  end)
  
  addcmd('fireproximityprompts',{'firepp'},function(args, speaker)
    if fireclickdetector then
      if args[1] then
        local name = getstring(1)
        for _, descendant in ipairs(workspace:GetDescendants()) do
          if descendant:IsA("ProximityPrompt") and descendant.Name == name or descandant.Parent.Name == name then
            fireproximityprompt(descendant)
          end
        end
      else
        for _, descendant in ipairs(workspace:GetDescendants()) do
          if descendant:IsA("ProximityPrompt") then
            fireproximityprompt(descendant)
          end
        end
      end
    else
      notify("Incompatible Exploit", "Your exploit does not support this command (missing fireproximityprompt)")
    end
  end)
  
  local PromptButtonHoldBegan = nil
  addcmd('instantproximityprompts',{'instantpp'},function(args, speaker)
    if fireproximityprompt then
      execCmd("uninstantproximityprompts")
      wait(0.1)
      PromptButtonHoldBegan = ProximityPromptService.PromptButtonHoldBegan:Connect(function(prompt)
        fireproximityprompt(prompt)
      end)
    else
      notify('Incompatible Exploit','Your exploit does not support this command (missing fireproximityprompt)')
    end
  end)
  
  addcmd('uninstantproximityprompts',{'uninstantpp'},function(args, speaker)
    if PromptButtonHoldBegan ~= nil then
      PromptButtonHoldBegan:Disconnect()
      PromptButtonHoldBegan = nil
    end
  end)
  
  addcmd('notifyping',{'ping'},function(args, speaker)
    notify("Ping", math.round(speaker:GetNetworkPing() * 1000) .. "ms")
  end)
  
  addcmd('grabtools', {}, function(args, speaker)
    local humanoid = speaker.Character:FindFirstChildWhichIsA("Humanoid")
    for _, child in ipairs(workspace:GetChildren()) do
      if speaker.Character and child:IsA("BackpackItem") and child:FindFirstChild("Handle") then
        humanoid:EquipTool(child)
      end
    end
  
    if grabtoolsFunc then 
      grabtoolsFunc:Disconnect() 
    end
  
    grabtoolsFunc = workspace.ChildAdded:Connect(function(child)
      if speaker.Character and child:IsA("BackpackItem") and child:FindFirstChild("Handle") then
        humanoid:EquipTool(child)
      end
    end)
  
    notify("Grabtools", "Picking up any dropped tools")
  end)
  
  addcmd('nograbtools',{'ungrabtools'},function(args, speaker)
    if grabtoolsFunc then 
      grabtoolsFunc:Disconnect() 
    end
  
    notify("Grabtools", "Grabtools has been disabled")
  end)
  
  local specifictoolremoval = {}
  addcmd('removespecifictool',{},function(args, speaker)
    if args[1] and speaker:FindFirstChildOfClass("Backpack") then
      local tool = string.lower(getstring(1))
      local RST = RunService.RenderStepped:Connect(function()
        if speaker:FindFirstChildOfClass("Backpack") then
          for i,v in pairs(speaker:FindFirstChildOfClass("Backpack"):GetChildren()) do
            if v.Name:lower() == tool then
              v:Remove()
            end
          end
        end
      end)
      specifictoolremoval[tool] = RST
    end
  end)
  
  addcmd('unremovespecifictool',{},function(args, speaker)
    if args[1] then
      local tool = string.lower(getstring(1))
      if specifictoolremoval[tool] ~= nil then
        specifictoolremoval[tool]:Disconnect()
        specifictoolremoval[tool] = nil
      end
    end
  end)
  
  addcmd('clearremovespecifictool',{},function(args, speaker)
    for obj in pairs(specifictoolremoval) do
      specifictoolremoval[obj]:Disconnect()
      specifictoolremoval[obj] = nil
    end
  end)
  
  addcmd('light',{},function(args, speaker)
    local light = Instance.new("PointLight")
    light.Parent = getRoot(speaker.Character)
    light.Range = 30
    if args[1] then
      light.Brightness = args[2]
      light.Range = args[1]
    else
      light.Brightness = 5
    end
  end)
  
  addcmd('unlight',{'nolight'},function(args, speaker)
    for i,v in pairs(speaker.Character:GetDescendants()) do
      if v.ClassName == "PointLight" then
        v:Destroy()
      end
    end
  end)
  
  addcmd('copytools',{},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players)do
      task.spawn(function()
        for i,v in pairs(Players[v]:FindFirstChildOfClass("Backpack"):GetChildren()) do
          if v:IsA('Tool') or v:IsA('HopperBin') then
            v:Clone().Parent = speaker:FindFirstChildOfClass("Backpack")
          end
        end
      end)
    end
  end)
  
  addcmd('naked',{},function(args, speaker)
    for i,v in pairs(speaker.Character:GetDescendants()) do
      if v:IsA("Clothing") or v:IsA("ShirtGraphic") then
        v:Destroy()
      end
    end
  end)
  
  addcmd('noface',{'removeface'},function(args, speaker)
    for i,v in pairs(speaker.Character:GetDescendants()) do
      if v:IsA("Decal") and v.Name == 'face' then
        v:Destroy()
      end
    end
  end)
  
  addcmd('spawnpoint',{'spawn'},function(args, speaker)
    spawnpos = getRoot(speaker.Character).CFrame
    spawnpoint = true
    spDelay = tonumber(args[1]) or 0.1
    notify('Spawn Point','Spawn point created at '..tostring(spawnpos))
  end)
  
  addcmd('nospawnpoint',{'nospawn','removespawnpoint'},function(args, speaker)
    spawnpoint = false
    notify('Spawn Point','Removed spawn point')
  end)
  
  addcmd('flashback',{'diedtp'},function(args, speaker)
    if lastDeath ~= nil then
      if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
        speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
        wait(.1)
      end
      getRoot(speaker.Character).CFrame = lastDeath
    end
  end)
  
  addcmd('hatspin',{'spinhats'},function(args, speaker)
    execCmd('unhatspin')
    wait(.5)
    for _,v in pairs(speaker.Character:FindFirstChildOfClass('Humanoid'):GetAccessories()) do
      local keep = Instance.new("BodyPosition") keep.Name = randomString() keep.Parent = v.Handle
      local spin = Instance.new("BodyAngularVelocity") spin.Name = randomString() spin.Parent = v.Handle
      v.Handle:FindFirstChildOfClass("Weld"):Destroy()
      if args[1] then
        spin.AngularVelocity = Vector3.new(0, args[1], 0)
        spin.MaxTorque = Vector3.new(0, args[1] * 2, 0)
      else
        spin.AngularVelocity = Vector3.new(0, 100, 0)
        spin.MaxTorque = Vector3.new(0, 200, 0)
      end
      keep.P = 30000
      keep.D = 50
      spinhats = RunService.Stepped:Connect(function()
        pcall(function()
          keep.Position = Players.LocalPlayer.Character.Head.Position
        end)
      end)
    end
  end)
  
  addcmd('unhatspin',{'unspinhats'},function(args, speaker)
    if spinhats then
      spinhats:Disconnect()
    end
    for _,v in pairs(speaker.Character:FindFirstChildOfClass('Humanoid'):GetAccessories()) do
      v.Parent = workspace
      for i,c in pairs(v.Handle) do
        if c:IsA("BodyPosition") or c:IsA("BodyAngularVelocity") then
          c:Destroy()
        end
      end
      wait()
      v.Parent = speaker.Character
    end
  end)
  
  addcmd('clearhats',{'cleanhats'},function(args, speaker)
    if firetouchinterest then
      local Player = Players.LocalPlayer
      local Character = Player.Character
      local Old = Character:FindFirstChild("HumanoidRootPart").CFrame
      local Hats = {}
  
      for _, child in ipairs(workspace:GetChildren()) do
        if child:IsA("Accessory") then
          table.insert(Hats, child)
        end
      end
  
      for _, accessory in ipairs(Character:FindFirstChildOfClass("Humanoid"):GetAccessories()) do
        accessory:Destroy()
      end
  
      for i = 1, #Hats do
        repeat RunService.Heartbeat:wait() until Hats[i]
        firetouchinterest(Hats[i].Handle,Character:FindFirstChild("HumanoidRootPart"),0)
        repeat RunService.Heartbeat:wait() until Character:FindFirstChildOfClass("Accessory")
        Character:FindFirstChildOfClass("Accessory"):Destroy()
        repeat RunService.Heartbeat:wait() until not Character:FindFirstChildOfClass("Accessory")
      end
  
      execCmd("reset")
  
      Player.CharacterAdded:Wait()
  
      for i = 1,20 do 
        RunService.Heartbeat:Wait()
        if Player.Character:FindFirstChild("HumanoidRootPart") then
          Player.Character:FindFirstChild("HumanoidRootPart").CFrame = Old
        end
      end
    else
      notify("Incompatible Exploit","Your exploit does not support this command (missing firetouchinterest)")
    end
  end)
  
  addcmd('vr',{},function(args, speaker)
    -- Full credit to Abacaxl @V3rmillion
    notify("Loading", "Hold on a sec")
    loadstring(game:HttpGet("https://raw.githubusercontent.com/infyiff/backup/main/vr.lua"))()
  end)
  
  addcmd('split',{},function(args, speaker)
    if r15(speaker) then
      speaker.Character.UpperTorso.Waist:Destroy()
    else
      notify('R15 Required','This command requires the r15 rig type')
    end
  end)
  
  addcmd('nilchar',{},function(args, speaker)
    if speaker.Character ~= nil then
      speaker.Character.Parent = nil
    end
  end)
  
  addcmd('unnilchar',{'nonilchar'},function(args, speaker)
    if speaker.Character ~= nil then
      speaker.Character.Parent = workspace
    end
  end)
  
  addcmd('noroot',{'removeroot','rroot'},function(args, speaker)
    if speaker.Character ~= nil then
      local char = Players.LocalPlayer.Character
      char.Parent = nil
      char.HumanoidRootPart:Destroy()
      char.Parent = workspace
    end
  end)
  
  addcmd('replaceroot',{'replacerootpart'},function(args, speaker)
    if speaker.Character ~= nil and speaker.Character:FindFirstChild("HumanoidRootPart") then
      local Char = speaker.Character
      local OldParent = Char.Parent
      local HRP = Char and Char:FindFirstChild("HumanoidRootPart")
      local OldPos = HRP.CFrame
      Char.Parent = game
      local HRP1 = HRP:Clone()
      HRP1.Parent = Char
      HRP = HRP:Destroy()
      HRP1.CFrame = OldPos
      Char.Parent = OldParent
    end
  end)
  
  addcmd('clearcharappearance',{'clearchar','clrchar'},function(args, speaker)
    speaker:ClearCharacterAppearance()
  end)
  
  addcmd('equiptools',{},function(args, speaker)
    for i,v in pairs(speaker:FindFirstChildOfClass("Backpack"):GetChildren()) do
      if v:IsA("Tool") or v:IsA("HopperBin") then
        v.Parent = speaker.Character
      end
    end
  end)
  
  addcmd('unequiptools',{},function(args, speaker)
    speaker.Character:FindFirstChildOfClass('Humanoid'):UnequipTools()
  end)
  
  local function GetHandleTools(p)
    p = p or Players.LocalPlayer
    local r = {}
    for _, v in ipairs(p.Character and p.Character:GetChildren() or {}) do
      if v.IsA(v, "BackpackItem") and v.FindFirstChild(v, "Handle") then
        r[#r + 1] = v
      end
    end
    for _, v in ipairs(p.Backpack:GetChildren()) do
      if v.IsA(v, "BackpackItem") and v.FindFirstChild(v, "Handle") then
        r[#r + 1] = v
      end
    end
    return r
  end
  addcmd('dupetools', {'clonetools'}, function(args, speaker)
    local LOOP_NUM = tonumber(args[1]) or 1
    local OrigPos = speaker.Character.HumanoidRootPart.Position
    local Tools, TempPos = {}, Vector3.new(math.random(-2e5, 2e5), 2e5, math.random(-2e5, 2e5))
    for i = 1, LOOP_NUM do
      local Human = speaker.Character:WaitForChild("Humanoid")
      wait(.1, Human.Parent:MoveTo(TempPos))
      Human.RootPart.Anchored = speaker:ClearCharacterAppearance(wait(.1)) or true
      local t = GetHandleTools(speaker)
      while #t > 0 do
        for _, v in ipairs(t) do
          task.spawn(function()
            for _ = 1, 25 do
              v.Parent = speaker.Character
              v.Handle.Anchored = true
            end
            for _ = 1, 5 do
              v.Parent = workspace
            end
            table.insert(Tools, v.Handle)
          end)
        end
        t = GetHandleTools(speaker)
      end
      wait(.1)
      speaker.Character = speaker.Character:Destroy()
      speaker.CharacterAdded:Wait():WaitForChild("Humanoid").Parent:MoveTo(LOOP_NUM == i and OrigPos or TempPos, wait(.1))
      if i == LOOP_NUM or i % 5 == 0 then
        local HRP = speaker.Character.HumanoidRootPart
        if type(firetouchinterest) == "function" then
          for _, v in ipairs(Tools) do
            v.Anchored = not firetouchinterest(v, HRP, 1, firetouchinterest(v, HRP, 0)) and false or false
          end
        else
          for _, v in ipairs(Tools) do
            task.spawn(function()
              local x = v.CanCollide
              v.CanCollide = false
              v.Anchored = false
              for _ = 1, 10 do
                v.CFrame = HRP.CFrame
                wait()
              end
              v.CanCollide = x
            end)
          end
        end
        wait(.1)
        Tools = {}
      end
      TempPos = TempPos + Vector3.new(10, math.random(-5, 5), 0)
    end
  end)
  
  local RS = RunService.RenderStepped
  addcmd('givetool', {'givetools'}, function(args, speaker)
    local v = Players[getPlayer(args[1], speaker)[1]].Character
    workspace.CurrentCamera.CameraSubject = v
    local Char = speaker.Character or workspace:FindFirstChild(speaker.Name)
    local hum = Char and Char:FindFirstChildWhichIsA('Humanoid')
    local hrp = hum and hum.RootPart
    local hrppos = hrp.CFrame
    hum = hum:Destroy() or hum:Clone()
    hum.Parent = Char
    hum:ClearAllChildren()
    speaker:ClearCharacterAppearance()
    task.spawn(function()
      speaker.CharacterAdded:Wait():WaitForChild('Humanoid').RootPart.CFrame = wait() and hrppos
    end)
    local vHRP = getRoot(v)
    while Char and Char.Parent and vHRP and vHRP.Parent do
      local Tools = false
      for _, v in ipairs(Char:GetChildren()) do
        if v:IsA('BackpackItem') and v:FindFirstChild('Handle') then
          Tools = true
          firetouchinterest(v.Handle, vHRP, 0)
          firetouchinterest(v.Handle, vHRP, 1)
        end
      end
      if not Tools then
        break
      end
      hrp.CFrame = vHRP.CFrame
      RS:Wait()
    end
    execCmd('re')
  end)
  
  addcmd('touchinterests', {'touchinterest', 'firetouchinterests', 'firetouchinterest'}, function(args, speaker)
    if not firetouchinterest then
      notify("Incompatible Exploit", "Your exploit does not support this command (missing firetouchinterest)")
      return
    end
  
    local root = getRoot(speaker.Character) or speaker.Character:FindFirstChildWhichIsA("BasePart")
  
    local function touch(x)
      x = x:FindFirstAncestorWhichIsA("Part")
      if x then
        if firetouchinterest then
          task.spawn(function()
            firetouchinterest(x, root, 1)
            wait()
            firetouchinterest(x, root, 0)
          end)
        end
        x.CFrame = root.CFrame
      end
    end
  
    if args[1] then
      local name = getstring(1)
      for _, descendant in ipairs(workspace:GetDescendants()) do
        if descendant:IsA("TouchTransmitter") and descendant.Name == name or descandant.Parent.Name == name then
          touch(descendant)
        end
      end
    else
      for _, descendant in ipairs(workspace:GetDescendants()) do
        if descendant:IsA("TouchTransmitter") then
          touch(descendant)
        end
      end
    end
  end)
  
  addcmd('fullbright',{'fb','fullbrightness'},function(args, speaker)
    Lighting.Brightness = 2
    Lighting.ClockTime = 14
    Lighting.FogEnd = 100000
    Lighting.GlobalShadows = false
    Lighting.OutdoorAmbient = Color3.fromRGB(128, 128, 128)
  end)
  
  addcmd('loopfullbright',{'loopfb'},function(args, speaker)
    if brightLoop then
      brightLoop:Disconnect()
    end
    local function brightFunc()
      Lighting.Brightness = 2
      Lighting.ClockTime = 14
      Lighting.FogEnd = 100000
      Lighting.GlobalShadows = false
      Lighting.OutdoorAmbient = Color3.fromRGB(128, 128, 128)
    end
  
    brightLoop = RunService.RenderStepped:Connect(brightFunc)
  end)
  
  addcmd('unloopfullbright',{'unloopfb'},function(args, speaker)
    if brightLoop then
      brightLoop:Disconnect()
    end
  end)
  
  addcmd('ambient',{},function(args, speaker)
    Lighting.Ambient = Color3.new(args[1],args[2],args[3])
    Lighting.OutdoorAmbient = Color3.new(args[1],args[2],args[3])
  end)
  
  addcmd('day',{},function(args, speaker)
    Lighting.ClockTime = 14
  end)
  
  addcmd('night',{},function(args, speaker)
    Lighting.ClockTime = 0
  end)
  
  addcmd('nofog',{},function(args, speaker)
    Lighting.FogEnd = 100000
    for i,v in pairs(Lighting:GetDescendants()) do
      if v:IsA("Atmosphere") then
        v:Destroy()
      end
    end
  end)
  
  addcmd('brightness',{},function(args, speaker)
    Lighting.Brightness = args[1]
  end)
  
  addcmd('globalshadows',{'gshadows'},function(args, speaker)
    Lighting.GlobalShadows = true
  end)
  
  addcmd('unglobalshadows',{'nogshadows','ungshadows','noglobalshadows'},function(args, speaker)
    Lighting.GlobalShadows = false
  end)
  
  origsettings = {abt = Lighting.Ambient, oabt = Lighting.OutdoorAmbient, brt = Lighting.Brightness, time = Lighting.ClockTime, fe = Lighting.FogEnd, fs = Lighting.FogStart, gs = Lighting.GlobalShadows}
  
  addcmd('restorelighting',{'rlighting'},function(args, speaker)
    Lighting.Ambient = origsettings.abt
    Lighting.OutdoorAmbient = origsettings.oabt
    Lighting.Brightness = origsettings.brt
    Lighting.ClockTime = origsettings.time
    Lighting.FogEnd = origsettings.fe
    Lighting.FogStart = origsettings.fs
    Lighting.GlobalShadows = origsettings.gs
  end)
  
  addcmd('stun',{'platformstand'},function(args, speaker)
    speaker.Character:FindFirstChildOfClass('Humanoid').PlatformStand = true
  end)
  
  addcmd('unstun',{'nostun','unplatformstand','noplatformstand'},function(args, speaker)
    speaker.Character:FindFirstChildOfClass('Humanoid').PlatformStand = false
  end)
  
  addcmd('norotate',{'noautorotate'},function(args, speaker)
    speaker.Character:FindFirstChildOfClass('Humanoid').AutoRotate  = false
  end)
  
  addcmd('unnorotate',{'autorotate'},function(args, speaker)
    speaker.Character:FindFirstChildOfClass('Humanoid').AutoRotate  = true
  end)
  
  addcmd('enablestate',{},function(args, speaker)
    local x = args[1]
    if not tonumber(x) then
      local x = Enum.HumanoidStateType[args[1]]
    end
    speaker.Character:FindFirstChildOfClass("Humanoid"):SetStateEnabled(x, true)
  end)
  
  addcmd('disablestate',{},function(args, speaker)
    local x = args[1]
    if not tonumber(x) then
      local x = Enum.HumanoidStateType[args[1]]
    end
    speaker.Character:FindFirstChildOfClass("Humanoid"):SetStateEnabled(x, false)
  end)
  
  addcmd('drophats',{'drophat'},function(args, speaker)
    if speaker.Character then
      for _,v in pairs(speaker.Character:FindFirstChildOfClass('Humanoid'):GetAccessories()) do
        v.Parent = workspace
      end
    end
  end)
  
  addcmd('deletehats',{'nohats','rhats'},function(args, speaker)
    for i,v in next, speaker.Character:GetDescendants() do
      if v:IsA("Accessory") then
        for i,p in next, v:GetDescendants() do
          if p:IsA("Weld") then
            p:Destroy()
          end
        end
      end
    end
  end)
  
  addcmd('droptools',{'droptool'},function(args, speaker)
    for i,v in pairs(Players.LocalPlayer.Backpack:GetChildren()) do
      if v:IsA("Tool") then
        v.Parent = Players.LocalPlayer.Character
      end
    end
    wait()
    for i,v in pairs(Players.LocalPlayer.Character:GetChildren()) do
      if v:IsA("Tool") then
        v.Parent = workspace
      end
    end
  end)
  
  addcmd('droppabletools',{},function(args, speaker)
    if speaker.Character then
      for _,obj in pairs(speaker.Character:GetChildren()) do
        if obj:IsA("Tool") then
          obj.CanBeDropped = true
        end
      end
    end
    if speaker:FindFirstChildOfClass("Backpack") then
      for _,obj in pairs(speaker:FindFirstChildOfClass("Backpack"):GetChildren()) do
        if obj:IsA("Tool") then
          obj.CanBeDropped = true
        end
      end
    end
  end)
  
  local currentToolSize = ""
  local currentGripPos = ""
  addcmd('reach',{},function(args, speaker)
    execCmd('unreach')
    wait()
    for i,v in pairs(speaker.Character:GetDescendants()) do
      if v:IsA("Tool") then
        if args[1] then
          currentToolSize = v.Handle.Size
          currentGripPos = v.GripPos
          local a = Instance.new("SelectionBox")
          a.Name = "SelectionBoxCreated"
          a.Parent = v.Handle
          a.Adornee = v.Handle
          v.Handle.Massless = true
          v.Handle.Size = Vector3.new(0.5,0.5,args[1])
          v.GripPos = Vector3.new(0,0,0)
          speaker.Character:FindFirstChildOfClass('Humanoid'):UnequipTools()
        else
          currentToolSize = v.Handle.Size
          currentGripPos = v.GripPos
          local a = Instance.new("SelectionBox")
          a.Name = "SelectionBoxCreated"
          a.Parent = v.Handle
          a.Adornee = v.Handle
          v.Handle.Massless = true
          v.Handle.Size = Vector3.new(0.5,0.5,60)
          v.GripPos = Vector3.new(0,0,0)
          speaker.Character:FindFirstChildOfClass('Humanoid'):UnequipTools()
        end
      end
    end
  end)
  
  addcmd('unreach',{'noreach'},function(args, speaker)
    for i,v in pairs(speaker.Character:GetDescendants()) do
      if v:IsA("Tool") then
        v.Handle.Size = currentToolSize
        v.GripPos = currentGripPos
        v.Handle.SelectionBoxCreated:Destroy()
      end
    end
  end)
  
  addcmd('grippos',{},function(args, speaker)
    for i,v in pairs(speaker.Character:GetDescendants()) do
      if v:IsA("Tool") then
        v.Parent = speaker:FindFirstChildOfClass("Backpack")
        v.GripPos = Vector3.new(args[1],args[2],args[3])
        v.Parent = speaker.Character
      end
    end
  end)
  
  addcmd('usetools', {}, function(args, speaker)
    local Backpack = speaker:FindFirstChildOfClass("Backpack")
    local ammount = tonumber(args[1]) or 1
    local delay_ = tonumber(args[2]) or false
    for _, v in ipairs(Backpack:GetChildren()) do
      v.Parent = speaker.Character
      task.spawn(function()
        for _ = 1, ammount do
          v:Activate()
          if delay_ then
            wait(delay_)
          end
        end
        v.Parent = Backpack
      end)
    end
  end)
  
  addcmd('logs',{},function(args, speaker)
    logs:TweenPosition(UDim2.new(0, 0, 1, -265), "InOut", "Quart", 0.3, true, nil)
  end)
  
  addcmd('chatlogs',{'clogs'},function(args, speaker)
    join.Visible = false
    chat.Visible = true
    table.remove(shade3,table.find(shade3,selectChat))
    table.remove(shade2,table.find(shade2,selectJoin))
    table.insert(shade2,selectChat)
    table.insert(shade3,selectJoin)
    selectJoin.BackgroundColor3 = currentShade3
    selectChat.BackgroundColor3 = currentShade2
    logs:TweenPosition(UDim2.new(0, 0, 1, -265), "InOut", "Quart", 0.3, true, nil)
  end)
  
  addcmd('joinlogs',{'jlogs'},function(args, speaker)
    chat.Visible = false
    join.Visible = true	
    table.remove(shade3,table.find(shade3,selectJoin))
    table.remove(shade2,table.find(shade2,selectChat))
    table.insert(shade2,selectJoin)
    table.insert(shade3,selectChat)
    selectChat.BackgroundColor3 = currentShade3
    selectJoin.BackgroundColor3 = currentShade2
    logs:TweenPosition(UDim2.new(0, 0, 1, -265), "InOut", "Quart", 0.3, true, nil)
  end)
  
  flinging = false
  addcmd('fling',{},function(args, speaker)
    flinging = false
    for _, child in pairs(speaker.Character:GetDescendants()) do
      if child:IsA("BasePart") then
        child.CustomPhysicalProperties = PhysicalProperties.new(math.huge, 0.3, 0.5)
      end
    end
    execCmd('noclip')
    wait(.1)
    local bambam = Instance.new("BodyAngularVelocity")
    bambam.Name = randomString()
    bambam.Parent = getRoot(speaker.Character)
    bambam.AngularVelocity = Vector3.new(0,99999,0)
    bambam.MaxTorque = Vector3.new(0,math.huge,0)
    bambam.P = math.huge
    local Char = speaker.Character:GetChildren()
    for i, v in next, Char do
      if v:IsA("BasePart") then
        v.CanCollide = false
        v.Massless = true
        v.Velocity = Vector3.new(0, 0, 0)
      end
    end
    flinging = true
    local function flingDiedF()
      execCmd('unfling')
    end
    flingDied = speaker.Character:FindFirstChildOfClass('Humanoid').Died:Connect(flingDiedF)
    repeat
      bambam.AngularVelocity = Vector3.new(0,99999,0)
      wait(.2)
      bambam.AngularVelocity = Vector3.new(0,0,0)
      wait(.1)
    until flinging == false
  end)
  
  addcmd('unfling',{'nofling'},function(args, speaker)
    execCmd('clip')
    if flingDied then
      flingDied:Disconnect()
    end
    flinging = false
    wait(.1)
    local speakerChar = speaker.Character
    if not speakerChar or not getRoot(speakerChar) then return end
    for i,v in pairs(getRoot(speakerChar):GetChildren()) do
      if v.ClassName == 'BodyAngularVelocity' then
        v:Destroy()
      end
    end
    for _, child in pairs(speakerChar:GetDescendants()) do
      if child.ClassName == "Part" or child.ClassName == "MeshPart" then
        child.CustomPhysicalProperties = PhysicalProperties.new(0.7, 0.3, 0.5)
      end
    end
  end)
  
  addcmd('togglefling',{},function(args, speaker)
    if flinging then
      execCmd('unfling')
    else
      execCmd('fling')
    end
  end)
  
  addcmd("flyfling", {}, function(args, speaker)
      execCmd("unvehiclefly\\unfling\\unnoclip")
      wait()
      execCmd("vehiclefly\\fling\\noclip")
  end)
  
  addcmd("unflyfling", {}, function(args, speaker)
      execCmd("unvehiclefly\\unfling\\unnoclip\\breakvelocity")
  end)
  
  addcmd("toggleflyfling", {}, function(args, speaker)
      execCmd(flinging and "unflyfling" or "flyfling")
  end)
  
  addcmd('invisfling',{},function(args, speaker)
    local ch = speaker.Character
    local prt=Instance.new("Model")
    prt.Parent = speaker.Character
    local z1 = Instance.new("Part")
    z1.Name="Torso"
    z1.CanCollide = false
    z1.Anchored = true
    local z2 = Instance.new("Part")
    z2.Name="Head"
    z2.Parent = prt
    z2.Anchored = true
    z2.CanCollide = false
    local z3 =Instance.new("Humanoid")
    z3.Name="Humanoid"
    z3.Parent = prt
    z1.Position = Vector3.new(0,9999,0)
    speaker.Character=prt
    wait(3)
    speaker.Character=ch
    wait(3)
    local Hum = Instance.new("Humanoid")
    z2:Clone()
    Hum.Parent = speaker.Character
    local root =  getRoot(speaker.Character)
    for i,v in pairs(speaker.Character:GetChildren()) do
      if v ~= root and  v.Name ~= "Humanoid" then
        v:Destroy()
      end
    end
    root.Transparency = 0
    root.Color = Color3.new(1, 1, 1)
    local invisflingStepped
    invisflingStepped = RunService.Stepped:Connect(function()
      if speaker.Character and getRoot(speaker.Character) then
        getRoot(speaker.Character).CanCollide = false
      else
        invisflingStepped:Disconnect()
      end
    end)
    sFLY()
    workspace.CurrentCamera.CameraSubject = root
    local bambam = Instance.new("BodyThrust")
    bambam.Parent = getRoot(speaker.Character)
    bambam.Force = Vector3.new(99999,99999*10,99999)
    bambam.Location = getRoot(speaker.Character).Position
  end)
  
  function attach(speaker,target)
    if tools(speaker) then
      local char = speaker.Character
      local tchar = target.Character
      local hum = speaker.Character:FindFirstChildOfClass("Humanoid")
      local hrp = getRoot(speaker.Character)
      local hrp2 = getRoot(target.Character)
      hum.Name = "1"
      local newHum = hum:Clone()
      newHum.Parent = char
      newHum.Name = "Humanoid"
      wait()
      hum:Destroy()
      workspace.CurrentCamera.CameraSubject = char
      newHum.DisplayDistanceType = "None"
      local tool = speaker:FindFirstChildOfClass("Backpack"):FindFirstChildOfClass("Tool") or speaker.Character:FindFirstChildOfClass("Tool")
      tool.Parent = char
      hrp.CFrame = hrp2.CFrame * CFrame.new(0, 0, 0) * CFrame.new(math.random(-100, 100)/200,math.random(-100, 100)/200,math.random(-100, 100)/200)
      local n = 0
      repeat
        wait(.1)
        n = n + 1
        hrp.CFrame = hrp2.CFrame
      until (tool.Parent ~= char or not hrp or not hrp2 or not hrp.Parent or not hrp2.Parent or n > 250) and n > 2
    else
      notify('Tool Required','You need to have an item in your inventory to use this command')
    end
  end
  
  addcmd('attach',{},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      attach(speaker,Players[v])
    end
  end)
  
  function kill(speaker,target,fast)
    if tools(speaker) then
      if target ~= nil then
        local NormPos = getRoot(speaker.Character).CFrame
        if not fast then
          refresh(speaker)
          wait()
          repeat wait() until speaker.Character ~= nil and getRoot(speaker.Character)
          wait(0.3)
        end
        local hrp = getRoot(speaker.Character)
        attach(speaker,target)
        repeat
          wait()
          hrp.CFrame = CFrame.new(999999, workspace.FallenPartsDestroyHeight + 5,999999)
        until not getRoot(target.Character) or not getRoot(speaker.Character)
        speaker.CharacterAdded:Wait():WaitForChild("HumanoidRootPart").CFrame = NormPos
      end
    else
      notify('Tool Required','You need to have an item in your inventory to use this command')
    end
  end
  
  addcmd('kill',{'fekill'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      kill(speaker,Players[v])
    end
  end)
  
  addcmd('handlekill', {'hkill'}, function(args, speaker)
    if not firetouchinterest then
      return notify('Incompatible Exploit', 'Your exploit does not support this command (missing firetouchinterest)')
    end
    local RS = RunService.RenderStepped
    local Tool = speaker.Character.FindFirstChildWhichIsA(speaker.Character, "Tool")
    local Handle = Tool and Tool.FindFirstChild(Tool, "Handle")
    if not Tool or not Handle then
      return notify("Handle Kill", "You need to hold a \"Tool\" that does damage on touch. For example the default \"Sword\" tool.")
    end
    for _, v in ipairs(getPlayer(args[1], speaker)) do
      v = Players[v]
      task.spawn(function()
        while Tool and speaker.Character and v.Character and Tool.Parent == speaker.Character do
          local Human = v.Character.FindFirstChildWhichIsA(v.Character, "Humanoid")
          if not Human or Human.Health <= 0 then
            break
          end
          for _, v1 in ipairs(v.Character.GetChildren(v.Character)) do
            v1 = ((v1.IsA(v1, "BasePart") and firetouchinterest(Handle, v1, 1, (RS.Wait(RS) and nil) or firetouchinterest(Handle, v1, 0)) and nil) or v1) or v1
          end
        end
        notify("Handle Kill Stopped!", v.Name .. " died/left or you unequipped the tool!")
      end)
    end
  end)
  
  local hb = RunService.Heartbeat
  addcmd('tpwalk', {'teleportwalk'}, function(args, speaker)
    tpwalking = true
    local chr = speaker.Character
    local hum = chr and chr:FindFirstChildWhichIsA("Humanoid")
    while tpwalking and chr and hum and hum.Parent do
      local delta = hb:Wait()
      if hum.MoveDirection.Magnitude > 0 then
        if args[1] and isNumber(args[1]) then
          chr:TranslateBy(hum.MoveDirection * tonumber(args[1]) * delta * 10)
        else
          chr:TranslateBy(hum.MoveDirection * delta * 10)
        end
      end
    end
  end)
  addcmd('untpwalk', {'unteleportwalk'}, function(args, speaker)
    tpwalking = false
  end)
  
  addcmd('fastkill',{'fastfekill'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      kill(speaker,Players[v],true)
    end
  end)
  
  function bring(speaker,target,fast)
    if tools(speaker) then
      if target ~= nil then
        local NormPos = getRoot(speaker.Character).CFrame
        if not fast then
          refresh(speaker)
          wait()
          repeat wait() until speaker.Character ~= nil and getRoot(speaker.Character)
          wait(0.3)
        end
        local hrp = getRoot(speaker.Character)
        attach(speaker,target)
        repeat
          wait()
          hrp.CFrame = NormPos
        until not getRoot(target.Character) or not getRoot(speaker.Character)
        speaker.CharacterAdded:Wait():WaitForChild("HumanoidRootPart").CFrame = NormPos
      end
    else
      notify('Tool Required','You need to have an item in your inventory to use this command')
    end
  end
  
  addcmd('bring',{'febring'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      bring(speaker,Players[v])
    end
  end)
  
  addcmd('fastbring',{'fastfebring'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      bring(speaker,Players[v],true)
    end
  end)
  
  function teleport(speaker,target,target2,fast)
    if tools(speaker) then
      if target ~= nil then
        local NormPos = getRoot(speaker.Character).CFrame
        if not fast then
          refresh(speaker)
          wait()
          repeat wait() until speaker.Character ~= nil and getRoot(speaker.Character)
          wait(0.3)
        end
        local hrp = getRoot(speaker.Character)
        local hrp2 = getRoot(target2.Character)
        attach(speaker,target)
        repeat
          wait()
          hrp.CFrame = hrp2.CFrame
        until not getRoot(target.Character) or not getRoot(speaker.Character)
        wait(1)
        speaker.CharacterAdded:Wait():WaitForChild("HumanoidRootPart").CFrame = NormPos
      end
    else
      notify('Tool Required','You need to have an item in your inventory to use this command')
    end
  end
  
  addcmd('tp',{'teleport'},function(args, speaker)
    local players1=getPlayer(args[1], speaker)
    local players2=getPlayer(args[2], speaker)
    for i,v in pairs(players1)do
      if getRoot(Players[v].Character) and getRoot(Players[players2[1]].Character) then
        if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
          speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
          wait(.1)
        end
        teleport(speaker,Players[v],Players[players2[1]])
      end
    end
  end)
  
  addcmd('fasttp',{'fastteleport'},function(args, speaker)
    local players1=getPlayer(args[1], speaker)
    local players2=getPlayer(args[2], speaker)
    for i,v in pairs(players1)do
      if getRoot(Players[v].Character) and getRoot(Players[players2[1]].Character) then
        if speaker.Character:FindFirstChildOfClass('Humanoid') and speaker.Character:FindFirstChildOfClass('Humanoid').SeatPart then
          speaker.Character:FindFirstChildOfClass('Humanoid').Sit = false
          wait(.1)
        end
        teleport(speaker,Players[v],Players[players2[1]],true)
      end
    end
  end)
  
  addcmd('spin',{},function(args, speaker)
    local spinSpeed = 20
    if args[1] and isNumber(args[1]) then
      spinSpeed = args[1]
    end
    for i,v in pairs(getRoot(speaker.Character):GetChildren()) do
      if v.Name == "Spinning" then
        v:Destroy()
      end
    end
    local Spin = Instance.new("BodyAngularVelocity")
    Spin.Name = "Spinning"
    Spin.Parent = getRoot(speaker.Character)
    Spin.MaxTorque = Vector3.new(0, math.huge, 0)
    Spin.AngularVelocity = Vector3.new(0,spinSpeed,0)
  end)
  
  addcmd('unspin',{},function(args, speaker)
    for i,v in pairs(getRoot(speaker.Character):GetChildren()) do
      if v.Name == "Spinning" then
        v:Destroy()
      end
    end
  end)
  
  xrayEnabled = false
  xray = function()
      for _, v in pairs(workspace:GetDescendants()) do
          if v:IsA("BasePart") and not v.Parent:FindFirstChildWhichIsA("Humanoid") and not v.Parent.Parent:FindFirstChildWhichIsA("Humanoid") then
              v.LocalTransparencyModifier = xrayEnabled and 0.5 or 0
          end
      end
  end
  
  addcmd("xray", {}, function(args, speaker)
      xrayEnabled = true
      xray()
  end)
  
  addcmd("unxray", {"noxray"}, function(args, speaker)
      xrayEnabled = false
      xray()
  end)
  
  addcmd("togglexray", {}, function(args, speaker)
      xrayEnabled = not xrayEnabled
      xray()
  end)
  
  addcmd("loopxray", {}, function(args, speaker)
      if xrayLoop then
          xrayLoop:Disconnect()
      end
      xrayLoop = RunService.RenderStepped:Connect(function()
          xrayEnabled = true
          xray()
      end)
  end)
  
  addcmd("unloopxray", {}, function(args, speaker)
      if xrayLoop then
          xrayLoop:Disconnect()
      end
  end)
  
  local walltpTouch = nil
  addcmd('walltp',{},function(args, speaker)
    local torso
    if r15(speaker) then
      torso = speaker.Character.UpperTorso
    else
      torso = speaker.Character.Torso
    end
    local function touchedFunc(hit)
      local Root = getRoot(speaker.Character)
      if hit:IsA("BasePart") and hit.Position.Y > Root.Position.Y - speaker.Character:FindFirstChildOfClass('Humanoid').HipHeight then
        local hitP = getRoot(hit.Parent)
        if hitP ~= nil then
          Root.CFrame = hit.CFrame * CFrame.new(Root.CFrame.lookVector.X,hitP.Size.Z/2 + speaker.Character:FindFirstChildOfClass('Humanoid').HipHeight,Root.CFrame.lookVector.Z)
        elseif hitP == nil then
          Root.CFrame = hit.CFrame * CFrame.new(Root.CFrame.lookVector.X,hit.Size.Y/2 + speaker.Character:FindFirstChildOfClass('Humanoid').HipHeight,Root.CFrame.lookVector.Z)
        end
      end
    end
    walltpTouch = torso.Touched:Connect(touchedFunc)
  end)
  
  addcmd('unwalltp',{'nowalltp'},function(args, speaker)
    if walltpTouch then
      walltpTouch:Disconnect()
    end
  end)
  
  autoclicking = false
  addcmd('autoclick',{},function(args, speaker)
    if mouse1press and mouse1release then
      execCmd('unautoclick')
      wait()
      local clickDelay = 0.1
      local releaseDelay = 0.1
      if args[1] and isNumber(args[1]) then clickDelay = args[1] end
      if args[2] and isNumber(args[2]) then releaseDelay = args[2] end
      autoclicking = true
      cancelAutoClick = UserInputService.InputBegan:Connect(function(input, gameProcessedEvent)
        if not gameProcessedEvent then
          if (input.KeyCode == Enum.KeyCode.Backspace and UserInputService:IsKeyDown(Enum.KeyCode.Equals)) or (input.KeyCode == Enum.KeyCode.Equals and UserInputService:IsKeyDown(Enum.KeyCode.Backspace)) then
            autoclicking = false
            cancelAutoClick:Disconnect()
          end
        end
      end)
      notify('Auto Clicker',"Press [backspace] and [=] at the same time to stop")
      repeat wait(clickDelay)
        mouse1press()
        wait(releaseDelay)
        mouse1release()
      until autoclicking == false
    else
      notify('Auto Clicker',"Your exploit doesn't have the ability to use the autoclick")
    end
  end)
  
  addcmd('unautoclick',{'noautoclick'},function(args, speaker)
    autoclicking = false
    if cancelAutoClick then cancelAutoClick:Disconnect() end
  end)
  
  addcmd('mousesensitivity',{'ms'},function(args, speaker)
    UserInputService.MouseDeltaSensitivity = args[1]
  end)
  
  local nameBox = nil
  local nbSelection = nil
  addcmd('hovername',{},function(args, speaker)
    execCmd('unhovername')
    wait()
    nameBox = Instance.new("TextLabel")
    nameBox.Name = randomString()
    nameBox.Parent = PARENT
    nameBox.BackgroundTransparency = 1
    nameBox.Size = UDim2.new(0,200,0,30)
    nameBox.Font = Enum.Font.Code
    nameBox.TextSize = 16
    nameBox.Text = ""
    nameBox.TextColor3 = Color3.new(1, 1, 1)
    nameBox.TextStrokeTransparency = 0
    nameBox.TextXAlignment = Enum.TextXAlignment.Left
    nameBox.ZIndex = 10
    nbSelection = Instance.new('SelectionBox')
    nbSelection.Name = randomString()
    nbSelection.LineThickness = 0.03
    nbSelection.Color3 = Color3.new(1, 1, 1)
    local function updateNameBox()
      local t
      local target = IYMouse.Target
  
      if target then
        local humanoid = target.Parent:FindFirstChildOfClass("Humanoid") or target.Parent.Parent:FindFirstChildOfClass("Humanoid")
        if humanoid then
          t = humanoid.Parent
        end
      end
  
      if t ~= nil then
        local x = IYMouse.X
        local y = IYMouse.Y
        local xP
        local yP
        if IYMouse.X > 200 then
          xP = x - 205
          nameBox.TextXAlignment = Enum.TextXAlignment.Right
        else
          xP = x + 25
          nameBox.TextXAlignment = Enum.TextXAlignment.Left
        end
        nameBox.Position = UDim2.new(0, xP, 0, y)
        nameBox.Text = t.Name
        nameBox.Visible = true
        nbSelection.Parent = t
        nbSelection.Adornee = t
      else
        nameBox.Visible = false
        nbSelection.Parent = nil
        nbSelection.Adornee = nil
      end
    end
    nbUpdateFunc = IYMouse.Move:Connect(updateNameBox)
  end)
  
  addcmd('unhovername',{'nohovername'},function(args, speaker)
    if nbUpdateFunc then
      nbUpdateFunc:Disconnect()
      nameBox:Destroy()
      nbSelection:Destroy()
    end
  end)
  
  addcmd('headsize',{},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      if Players[v] ~= speaker and Players[v].Character:FindFirstChild('Head') then
        local sizeArg = tonumber(args[2])
        local Size = Vector3.new(sizeArg,sizeArg,sizeArg)
        local Head = Players[v].Character:FindFirstChild('Head')
        if Head:IsA("BasePart") then
          if not args[2] or sizeArg == 1 then
            Head.Size = Vector3.new(2,1,1)
          else
            Head.Size = Size
          end
        end
      end
    end
  end)
  
  addcmd('hitbox',{},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      if Players[v] ~= speaker and Players[v].Character:FindFirstChild('HumanoidRootPart') then
        local sizeArg = tonumber(args[2])
        local Size = Vector3.new(sizeArg,sizeArg,sizeArg)
        local Root = Players[v].Character:FindFirstChild('HumanoidRootPart')
        if Root:IsA("BasePart") then
          if not args[2] or sizeArg == 1 then
            Root.Size = Vector3.new(2,1,1)
            Root.Transparency = 0.4
          else
            Root.Size = Size
            Root.Transparency = 0.4
          end
        end
      end
    end
  end)
  
  addcmd('stareat',{'stare'},function(args, speaker)
    local players = getPlayer(args[1], speaker)
    for i,v in pairs(players) do
      if stareLoop then
        stareLoop:Disconnect()
      end
      if not Players.LocalPlayer.Character:FindFirstChild("HumanoidRootPart") and Players[v].Character:FindFirstChild("HumanoidRootPart") then return end
      local function stareFunc()
        if Players.LocalPlayer.Character.PrimaryPart and Players:FindFirstChild(v) and Players[v].Character ~= nil and Players[v].Character:FindFirstChild("HumanoidRootPart") then
          local chrPos=Players.LocalPlayer.Character.PrimaryPart.Position
          local tPos=Players[v].Character:FindFirstChild("HumanoidRootPart").Position
          local modTPos=Vector3.new(tPos.X,chrPos.Y,tPos.Z)
          local newCF=CFrame.new(chrPos,modTPos)
          Players.LocalPlayer.Character:SetPrimaryPartCFrame(newCF)
        elseif not Players:FindFirstChild(v) then
          stareLoop:Disconnect()
        end
      end
  
      stareLoop = RunService.RenderStepped:Connect(stareFunc)
    end
  end)
  
  addcmd('unstareat',{'unstare','nostare','nostareat'},function(args, speaker)
    if stareLoop then
      stareLoop:Disconnect()
    end
  end)
  
  RolewatchData = {Group = 0, Role = "", Leave = false}
  RolewatchConnection = Players.PlayerAdded:Connect(function(player)
    if RolewatchData.Group == 0 then return end
    if player:IsInGroup(RolewatchData.Group) then
      if tostring(player:GetRoleInGroup(RolewatchData.Group)):lower() == RolewatchData.Role:lower() then
        if RolewatchData.Leave == true then
          Players.LocalPlayer:Kick("\n\nRolewatch\nPlayer \"" .. tostring(player.Name) .. "\" has joined with the Role \"" .. RolewatchData.Role .. "\"\n")
        else
          notify("Rolewatch", "Player \"" .. tostring(player.Name) .. "\" has joined with the Role \"" .. RolewatchData.Role .. "\"")
        end
      end
    end
  end)
  
  addcmd("rolewatch", {}, function(args, speaker)
      local groupId = tonumber(args[1] or 0)
      local roleName = args[2] and tostring(getstring(2))
      if groupId and roleName then
          RolewatchData.Group = groupId
          RolewatchData.Role = roleName
          notify("Rolewatch", "Watching Group ID \"" .. tostring(groupId) .. "\" for Role \"" .. roleName .. "\"")
      end
  end)
  
  addcmd("rolewatchstop", {}, function(args, speaker)
      RolewatchData.Group = 0
      RolewatchData.Role = ""
      RolewatchData.Leave = false
      notify("Rolewatch", "Disabled")
  end)
  
  addcmd("rolewatchleave", {"unrolewatch"}, function(args, speaker)
      RolewatchData.Leave = not RolewatchData.Leave
      notify("Rolewatch", RolewatchData.Leave and "Leave has been Enabled" or "Leave has been Disabled")
  end)
  
  staffRoles = {"mod", "admin", "staff", "dev", "founder", "owner", "supervis", "manager", "management", "executive", "president", "chairman", "chairwoman", "chairperson", "director"}
  
  getStaffRole = function(player)
      local playerRole = player:GetRoleInGroup(game.CreatorId)
      local result = {Role = playerRole, Staff = false}
      for _, role in pairs(staffRoles) do
          if string.find(string.lower(playerRole), role) then
              result.Staff = true
          end
      end
      return result
  end
  
  addcmd("staffwatch", {}, function(args, speaker)
      if staffwatchjoin then
          staffwatchjoin:Disconnect()
      end
      if game.CreatorType == Enum.CreatorType.Group then
          local found = {}
          staffwatchjoin = Players.PlayerAdded:Connect(function(player)
              local result = getStaffRole(player)
              if result.Staff then
                  notify("Staffwatch", formatUsername(player) .. " is a " .. result.Role)
              end
          end)
          for _, player in pairs(Players:GetPlayers()) do
              local result = getStaffRole(player)
              if result.Staff then
                  table.insert(found, formatUsername(player) .. " is a " .. result.Role)
              end
          end
          if #found > 0 then
              notify("Staffwatch", table.concat(found, ",\n"))
          else
              notify("Staffwatch", "Enabled")
          end
      else
          notify("Staffwatch", "Game is not owned by a Group")
      end
  end)
  
  addcmd("unstaffwatch", {}, function(args, speaker)
      if staffwatchjoin then
          staffwatchjoin:Disconnect()
      end
      notify("Staffwatch", "Disabled")
  end)
  
  addcmd('removeterrain',{'rterrain','noterrain'},function(args, speaker)
    workspace:FindFirstChildOfClass('Terrain'):Clear()
  end)
  
  addcmd('clearnilinstances',{'nonilinstances','cni'},function(args, speaker)
    if getnilinstances then
      for i,v in pairs(getnilinstances()) do
        v:Destroy()
      end
    else
      notify('Incompatible Exploit','Your exploit does not support this command (missing getnilinstances)')
    end
  end)
  
  addcmd('destroyheight',{'dh'},function(args, speaker)
    local dh = args[1] or -500
    if isNumber(dh) then
      workspace.FallenPartsDestroyHeight = dh
    end
  end)
  
  addcmd('trip',{},function(args, speaker)
    if speaker and speaker.Character and speaker.Character:FindFirstChildOfClass("Humanoid") and getRoot(speaker.Character) then
      local hum = speaker.Character:FindFirstChildOfClass("Humanoid")
      local root = getRoot(speaker.Character)
      hum:ChangeState(0)
      root.Velocity = root.CFrame.LookVector * 30
    end
  end)
  
  local freezingua = nil
  frozenParts = {}
  addcmd('freezeunanchored',{'freezeua'},function(args, speaker)
    if sethidden then
      local badnames = {
        "Head",
        "UpperTorso",
        "LowerTorso",
        "RightUpperArm",
        "LeftUpperArm",
        "RightLowerArm",
        "LeftLowerArm",
        "RightHand",
        "LeftHand",
        "RightUpperLeg",
        "LeftUpperLeg",
        "RightLowerLeg",
        "LeftLowerLeg",
        "RightFoot",
        "LeftFoot",
        "Torso",
        "Right Arm",
        "Left Arm",
        "Right Leg",
        "Left Leg",
        "HumanoidRootPart"
      }
      local function FREEZENOOB(v)
        if v:IsA("BasePart" or "UnionOperation") and v.Anchored == false then
          local BADD = false
          for i = 1,#badnames do
            if v.Name == badnames[i] then
              BADD = true
            end
          end
          if speaker.Character and v:IsDescendantOf(speaker.Character) then
            BADD = true
          end
          if BADD == false then
            for i,c in pairs(v:GetChildren()) do
              if c:IsA("BodyPosition") or c:IsA("BodyGyro") then
                c:Destroy()
              end
            end
            local bodypos = Instance.new("BodyPosition")
            bodypos.Parent = v
            bodypos.Position = v.Position
            bodypos.MaxForce = Vector3.new(math.huge,math.huge,math.huge)
            local bodygyro = Instance.new("BodyGyro")
            bodygyro.Parent = v
            bodygyro.CFrame = v.CFrame
            bodygyro.MaxTorque = Vector3.new(math.huge,math.huge,math.huge)
            if not table.find(frozenParts,v) then
              table.insert(frozenParts,v)
            end
          end
        end
      end
      for i,v in pairs(workspace:GetDescendants()) do
        FREEZENOOB(v)
      end
      freezingua = workspace.DescendantAdded:Connect(FREEZENOOB)
    else
      notify('Incompatible Exploit','Your exploit does not support this command (missing sethiddenproperty)')
    end
  end)
  
  addcmd('thawunanchored',{'thawua','unfreezeunanchored','unfreezeua'},function(args, speaker)
    if sethidden then
      if freezingua then
        freezingua:Disconnect()
      end
      for i,v in pairs(frozenParts) do
        for i,c in pairs(v:GetChildren()) do
          if c:IsA("BodyPosition") or c:IsA("BodyGyro") then
            c:Destroy()
          end
        end
      end
      frozenParts = {}
    else
      notify('Incompatible Exploit','Your exploit does not support this command (missing sethiddenproperty)')
    end
  end)
  
  addcmd('tpunanchored',{'tpua'},function(args, speaker)
    if sethidden then
      local players = getPlayer(args[1], speaker)
      for i,v in pairs(players) do
        local Forces = {}
        for _,part in pairs(workspace:GetDescendants()) do
          if Players[v].Character:FindFirstChild('Head') and part:IsA("BasePart" or "UnionOperation" or "Model") and part.Anchored == false and not part:IsDescendantOf(speaker.Character) and part.Name == "Torso" == false and part.Name == "Head" == false and part.Name == "Right Arm" == false and part.Name == "Left Arm" == false and part.Name == "Right Leg" == false and part.Name == "Left Leg" == false and part.Name == "HumanoidRootPart" == false then
            for i,c in pairs(part:GetChildren()) do
              if c:IsA("BodyPosition") or c:IsA("BodyGyro") then
                c:Destroy()
              end
            end
            local ForceInstance = Instance.new("BodyPosition")
            ForceInstance.Parent = part
            ForceInstance.MaxForce = Vector3.new(math.huge, math.huge, math.huge)
            table.insert(Forces, ForceInstance)
            if not table.find(frozenParts,part) then
              table.insert(frozenParts,part)
            end
          end
        end
        for i,c in pairs(Forces) do
          c.Position = Players[v].Character.Head.Position
        end
      end
    else
      notify('Incompatible Exploit','Your exploit does not support this command (missing sethiddenproperty)')
    end
  end)
  
  keycodeMap = {
    ["0"] = 0x30,
    ["1"] = 0x31,
    ["2"] = 0x32,
    ["3"] = 0x33,
    ["4"] = 0x34,
    ["5"] = 0x35,
    ["6"] = 0x36,
    ["7"] = 0x37,
    ["8"] = 0x38,
    ["9"] = 0x39,
    ["a"] = 0x41,
    ["b"] = 0x42,
    ["c"] = 0x43,
    ["d"] = 0x44,
    ["e"] = 0x45,
    ["f"] = 0x46,
    ["g"] = 0x47,
    ["h"] = 0x48,
    ["i"] = 0x49,
    ["j"] = 0x4A,
    ["k"] = 0x4B,
    ["l"] = 0x4C,
    ["m"] = 0x4D,
    ["n"] = 0x4E,
    ["o"] = 0x4F,
    ["p"] = 0x50,
    ["q"] = 0x51,
    ["r"] = 0x52,
    ["s"] = 0x53,
    ["t"] = 0x54,
    ["u"] = 0x55,
    ["v"] = 0x56,
    ["w"] = 0x57,
    ["x"] = 0x58,
    ["y"] = 0x59,
    ["z"] = 0x5A,
    ["enter"] = 0x0D,
    ["shift"] = 0x10,
    ["ctrl"] = 0x11,
    ["alt"] = 0x12,
    ["pause"] = 0x13,
    ["capslock"] = 0x14,
    ["spacebar"] = 0x20,
    ["space"] = 0x20,
    ["pageup"] = 0x21,
    ["pagedown"] = 0x22,
    ["end"] = 0x23,
    ["home"] = 0x24,
    ["left"] = 0x25,
    ["up"] = 0x26,
    ["right"] = 0x27,
    ["down"] = 0x28,
    ["insert"] = 0x2D,
    ["delete"] = 0x2E,
    ["f1"] = 0x70,
    ["f2"] = 0x71,
    ["f3"] = 0x72,
    ["f4"] = 0x73,
    ["f5"] = 0x74,
    ["f6"] = 0x75,
    ["f7"] = 0x76,
    ["f8"] = 0x77,
    ["f9"] = 0x78,
    ["f10"] = 0x79,
    ["f11"] = 0x7A,
    ["f12"] = 0x7B,
  }
  autoKeyPressing = false
  cancelAutoKeyPress = nil
  
  addcmd('autokeypress',{'keypress'},function(args, speaker)
    if keypress and keyrelease and args[1] then
      local code = keycodeMap[args[1]:lower()]
      if not code then notify('Auto Key Press',"Invalid key") return end
      execCmd('unautokeypress')
      wait()
      local clickDelay = 0.1
      local releaseDelay = 0.1
      if args[2] and isNumber(args[2]) then clickDelay = args[2] end
      if args[3] and isNumber(args[3]) then releaseDelay = args[3] end
      autoKeyPressing = true
      cancelAutoKeyPress = UserInputService.InputBegan:Connect(function(input, gameProcessedEvent)
        if not gameProcessedEvent then
          if (input.KeyCode == Enum.KeyCode.Backspace and UserInputService:IsKeyDown(Enum.KeyCode.Equals)) or (input.KeyCode == Enum.KeyCode.Equals and UserInputService:IsKeyDown(Enum.KeyCode.Backspace)) then
            autoKeyPressing = false
            cancelAutoKeyPress:Disconnect()
          end
        end
      end)
      notify('Auto Key Press',"Press [backspace] and [=] at the same time to stop")
      repeat wait(clickDelay)
        keypress(code)
        wait(releaseDelay)
        keyrelease(code)
      until autoKeyPressing == false
      if cancelAutoKeyPress then cancelAutoKeyPress:Disconnect() keyrelease(code) end
    else
      notify('Auto Key Press',"Your exploit doesn't have the ability to use auto key press")
    end
  end)
  
  addcmd('unautokeypress',{'noautokeypress','unkeypress','nokeypress'},function(args, speaker)
    autoKeyPressing = false
    if cancelAutoKeyPress then cancelAutoKeyPress:Disconnect() end
  end)
  
  addcmd('addplugin',{'plugin'},function(args, speaker)
    addPlugin(getstring(1))
  end)
  
  addcmd('removeplugin',{'deleteplugin'},function(args, speaker)
    deletePlugin(getstring(1))
  end)
  
  addcmd('reloadplugin',{},function(args, speaker)
    local pluginName = getstring(1)
    deletePlugin(pluginName)
    wait(1)
    addPlugin(pluginName)
  end)
  
  addcmd('removecmd',{'deletecmd'},function(args, speaker)
    removecmd(args[1])
  end)
  
  if IsOnMobile then
    local QuickCapture = Instance.new("TextButton")
    local UICorner = Instance.new("UICorner")
    QuickCapture.Name = randomString()
    QuickCapture.Parent = PARENT
    QuickCapture.BackgroundColor3 = Color3.fromRGB(46, 46, 47)
    QuickCapture.BackgroundTransparency = 0.14
    QuickCapture.Position = UDim2.new(0.489, 0, 0, 0)
    QuickCapture.Size = UDim2.new(0, 32, 0, 33)
    QuickCapture.Font = Enum.Font.SourceSansBold
    QuickCapture.Text = "IY"
    QuickCapture.TextColor3 = Color3.fromRGB(255, 255, 255)
    QuickCapture.TextSize = 20.000
    QuickCapture.TextWrapped = true
    QuickCapture.Draggable = true
    UICorner.Name = randomString()
    UICorner.CornerRadius = UDim.new(0.5, 0)
    UICorner.Parent = QuickCapture
    QuickCapture.MouseButton1Click:Connect(function()
      Cmdbar:CaptureFocus()
      maximizeHolder()
    end)
    table.insert(shade1, QuickCapture)
    table.insert(text1, QuickCapture)
  end
  
  updateColors(currentShade1,shade1)
  updateColors(currentShade2,shade2)
  updateColors(currentShade3,shade3)
  updateColors(currentText1,text1)
  updateColors(currentText2,text2)
  updateColors(currentScroll,scroll)
  
  if PluginsTable ~= nil or PluginsTable ~= {} then
    FindPlugins(PluginsTable)
  end
  
  -- Events
  eventEditor.RegisterEvent("OnExecute")
  eventEditor.RegisterEvent("OnSpawn",{
    {Type="Player",Name="Player Filter ($1)"}
  })
  eventEditor.RegisterEvent("OnDied",{
    {Type="Player",Name="Player Filter ($1)"}
  })
  eventEditor.RegisterEvent("OnDamage",{
    {Type="Player",Name="Player Filter ($1)"},
    {Type="Number",Name="Below Health ($2)"}
  })
  eventEditor.RegisterEvent("OnKilled",{
    {Type="Player",Name="Victim Player ($1)"},
    {Type="Player",Name="Killer Player ($2)",Default = 1}
  })
  eventEditor.RegisterEvent("OnJoin",{
    {Type="Player",Name="Player Filter ($1)",Default = 1}
  })
  eventEditor.RegisterEvent("OnLeave",{
    {Type="Player",Name="Player Filter ($1)",Default = 1}
  })
  eventEditor.RegisterEvent("OnChatted",{
    {Type="Player",Name="Player Filter ($1)",Default = 1},
    {Type="String",Name="Message Filter ($2)"}
  })
  
  function hookCharEvents(plr,instant)
    task.spawn(function()
      local char = plr.Character
      if not char then return end
  
      local humanoid = char:WaitForChild("Humanoid",10)
      if not humanoid then return end
  
      local oldHealth = humanoid.Health
      humanoid.HealthChanged:Connect(function(health)
        local change = math.abs(oldHealth - health)
        if oldHealth > health then
          eventEditor.FireEvent("OnDamage",plr.Name,tonumber(health))
        end
        oldHealth = health
      end)
  
      humanoid.Died:Connect(function()
        eventEditor.FireEvent("OnDied",plr.Name)
  
        local killedBy = humanoid:FindFirstChild("creator")
        if killedBy and killedBy.Value and killedBy.Value.Parent then
          eventEditor.FireEvent("OnKilled",plr.Name,killedBy.Name)
        end
      end)
    end)
  end
  
  Players.PlayerAdded:Connect(function(plr)
    eventEditor.FireEvent("OnJoin",plr.Name)
    plr.Chatted:Connect(function(msg) eventEditor.FireEvent("OnChatted",tostring(plr),msg) end)
    plr.CharacterAdded:Connect(function() eventEditor.FireEvent("OnSpawn",tostring(plr)) hookCharEvents(plr) end)
    JoinLog(plr)
    ChatLog(plr)
    if ESPenabled then
      repeat wait(1) until plr.Character and getRoot(plr.Character)
      ESP(plr)
    end
    if CHMSenabled then
      repeat wait(1) until plr.Character and getRoot(plr.Character)
      CHMS(plr)
    end
  end)
  
  for _,plr in pairs(Players:GetPlayers()) do
    pcall(function()
      plr.CharacterAdded:Connect(function() eventEditor.FireEvent("OnSpawn",tostring(plr)) hookCharEvents(plr) end)
      hookCharEvents(plr)
    end)
  end
  
  if spawnCmds and #spawnCmds > 0 then
    for i,v in pairs(spawnCmds) do
      eventEditor.AddCmd("OnSpawn",{v.COMMAND or "",{0},v.DELAY or 0})
    end
    updatesaves()
  end
  
  if loadedEventData then eventEditor.LoadData(loadedEventData) end
  eventEditor.Refresh()
  
  eventEditor.FireEvent("OnExecute")
  
  if aliases and #aliases > 0 then
    local cmdMap = {}
    for i,v in pairs(cmds) do
      cmdMap[v.NAME:lower()] = v
      for _,alias in pairs(v.ALIAS) do
        cmdMap[alias:lower()] = v
      end
    end
    for i = 1, #aliases do
      local cmd = string.lower(aliases[i].CMD)
      local alias = string.lower(aliases[i].ALIAS)
      if cmdMap[cmd] then
        customAlias[alias] = cmdMap[cmd]
      end
    end
    refreshaliases()
  end
  
  IYMouse.Move:Connect(checkTT)
  
  task.spawn(function()
    local success, latestVersionInfo = pcall(function() 
      local versionJson = game:HttpGet('https://raw.githubusercontent.com/EdgeIY/infiniteyield/master/version')
      return HttpService:JSONDecode(versionJson)
    end)
  
    if success then
      if currentVersion ~= latestVersionInfo.Version then
        notify('Outdated','Get the new version at infyiff.github.io')
      end
  
      if latestVersionInfo.Announcement and latestVersionInfo.Announcement ~= '' then
        local AnnGUI = Instance.new("Frame")
        local background = Instance.new("Frame")
        local TextBox = Instance.new("TextLabel")
        local shadow = Instance.new("Frame")
        local PopupText = Instance.new("TextLabel")
        local Exit = Instance.new("TextButton")
        local ExitImage = Instance.new("ImageLabel")
  
        AnnGUI.Name = randomString()
        AnnGUI.Parent = PARENT
        AnnGUI.Active = true
        AnnGUI.BackgroundTransparency = 1
        AnnGUI.Position = UDim2.new(0.5, -180, 0, -500)
        AnnGUI.Size = UDim2.new(0, 360, 0, 20)
        AnnGUI.ZIndex = 10
  
        background.Name = "background"
        background.Parent = AnnGUI
        background.Active = true
        background.BackgroundColor3 = currentShade1
        background.BorderSizePixel = 0
        background.Position = UDim2.new(0, 0, 0, 20)
        background.Size = UDim2.new(0, 360, 0, 150)
        background.ZIndex = 10
  
        TextBox.Parent = background
        TextBox.BackgroundTransparency = 1
        TextBox.Position = UDim2.new(0, 5, 0, 5)
        TextBox.Size = UDim2.new(0, 350, 0, 140)
        TextBox.Font = Enum.Font.SourceSans
        TextBox.TextSize = 18
        TextBox.TextWrapped = true
        TextBox.Text = Announcement
        TextBox.TextColor3 = currentText1
        TextBox.TextXAlignment = Enum.TextXAlignment.Left
        TextBox.TextYAlignment = Enum.TextYAlignment.Top
        TextBox.ZIndex = 10
  
        shadow.Name = "shadow"
        shadow.Parent = AnnGUI
        shadow.BackgroundColor3 = currentShade2
        shadow.BorderSizePixel = 0
        shadow.Size = UDim2.new(0, 360, 0, 20)
        shadow.ZIndex = 10
  
        PopupText.Name = "PopupText"
        PopupText.Parent = shadow
        PopupText.BackgroundTransparency = 1
        PopupText.Size = UDim2.new(1, 0, 0.95, 0)
        PopupText.ZIndex = 10
        PopupText.Font = Enum.Font.SourceSans
        PopupText.TextSize = 14
        PopupText.Text = "Server Announcement"
        PopupText.TextColor3 = currentText1
        PopupText.TextWrapped = true
  
        Exit.Name = "Exit"
        Exit.Parent = shadow
        Exit.BackgroundTransparency = 1
        Exit.Position = UDim2.new(1, -20, 0, 0)
        Exit.Size = UDim2.new(0, 20, 0, 20)
        Exit.Text = ""
        Exit.ZIndex = 10
  
        ExitImage.Parent = Exit
        ExitImage.BackgroundColor3 = Color3.new(1, 1, 1)
        ExitImage.BackgroundTransparency = 1
        ExitImage.Position = UDim2.new(0, 5, 0, 5)
        ExitImage.Size = UDim2.new(0, 10, 0, 10)
        ExitImage.Image = "rbxassetid://5054663650"
        ExitImage.ZIndex = 10
  
        wait(1)
        AnnGUI:TweenPosition(UDim2.new(0.5, -180, 0, 150), "InOut", "Quart", 0.5, true, nil)
  
        Exit.MouseButton1Click:Connect(function()
          AnnGUI:TweenPosition(UDim2.new(0.5, -180, 0, -500), "InOut", "Quart", 0.5, true, nil)
          wait(0.6)
          AnnGUI:Destroy()
        end)
      end
    end
  end)
  
  task.spawn(function()
    wait()
    Credits:TweenPosition(UDim2.new(0, 0, 0.9, 0), "Out", "Quart", 0.2)
    Logo:TweenSizeAndPosition(UDim2.new(0, 175, 0, 175), UDim2.new(0, 37, 0, 45), "Out", "Quart", 0.3)
    wait(1)
    local OutInfo = TweenInfo.new(1.6809, Enum.EasingStyle.Sine, Enum.EasingDirection.Out, 0, false, 0)
    TweenService:Create(Logo, OutInfo, {ImageTransparency = 1}):Play()
    TweenService:Create(IntroBackground, OutInfo, {BackgroundTransparency = 1}):Play()
    Credits:TweenPosition(UDim2.new(0, 0, 0.9, 30), "Out", "Quart", 0.2)
    wait(0.2)
    Logo:Destroy()
    Credits:Destroy()
    IntroBackground:Destroy()
    minimizeHolder()
    if IsOnMobile then notify("Unstable Device", "On mobile, Infinite Yield may have issues or features that are not functioning correctly.") end
  end)
  
  end
	environment.global.getexecutorversion = environment.global.bincogver

	environment.global.cloneref = function(reference)
		assert(reference, "Missing #1 argument")
		assert(typeof(reference) == "Instance", "Expected #1 argument to be Instance, got "..tostring(typeof(reference)).." instead")
		getfenv().clonerefed = true
		if game:FindFirstChild(reference.Name)  or reference.Parent == game then --  dont make it clone services
			return reference
		else
			local class = reference.ClassName

			local cloned = Instance.new(class)

			local mt = {
				__index = reference,
				__newindex = function(t, k, v)

					if k == "Name" then
						reference.Name = v
					end
					rawset(t, k, v)
				end
			}

			local proxy = setmetatable({}, mt)

			return proxy
		end

	end

	environment.global.compareinstances = function()
		if clonerefed then return true else return false end
	end

	-- cache stuff

	local CachedInstances = {}

	environment.global.cache.iscached = function(d)
				return CachedInstances[d] ~= 'invalid'
	end
	environment.global.cache.invalidate = function(d)
		CachedInstances[d] = 'invalid'
		d.Parent = nil
	end
	environment.global.cache.replace = function(a, b)
		CachedInstances[a] = b
		b.Name = a.Name
		b.Parent = a.Parent
		a.Parent = nil
	end

	-- END OF CACHE

	
	environment.global.setclipboard = function(data)
		assert(data, "Missing #1 argument")

		bridge:send("setclipboard", tostring(data))
	end
	environment.global.getclipboard = function()
		return bridge:send("getclipboard").message
	end
	local hookfunction = function(a,b) 
		if not rawget(getfenv(), a) and a ~= nil and type(a) == "function" then
			local c = debug.info(a, "n")
			a = c ~= "" and c or a
		end
		if type(a) == "function" or type(a) == "string" then 
			for i,v in pairs(getfenv()) do
				if v == a then
					a = i 
				end
			end
			local c = rawset(getfenv(), a, b)
			return rawget(getfenv(), a) or a 
		elseif type(a) == "table" then
			local tbl = getfenv()
			local hooking = ""
			getfenv()[a[1]] = table.clone(getfenv()[a[1]])
			for i=1,#a do
				tbl = tbl[a[i]]
				hooking = hooking ~= "" and hooking.."."..tostring(a[i]) or tostring(a[i])
				if i == #a then
					getfenv()[a[i-1]][a[i]] = b
					return getfenv()[a[i-1]][a[i]]
				end
			end
		end
	end
	local _setmtbl = setmetatable
	local metatbl = {}
	hookfunction(setmetatable, function(a, b)
    	local c, d = pcall(function()
        	local c = _setmtbl(a, b)
    	end)
    	metatbl[a] = b
    	return a
	end)
	
	environment.global.getrawmetatable = function(object)
    	return metatbl[object]
	end
	environment.global.setrawmetatable = function(object, metatable)
		local mtbl = getrawmetatable(object)
		table.foreach(metatable, function(some, thing)
			mtbl[some] = thing
		end)
		return object
	end


	environment.global.toclipboard = environment.global.setclipboard
	environment.global.setrbxclipboard = environment.global.setclipboard
	environment.global.syn.write_clipboard = environment.global.setclipboard
	environment.global.writeclipboard = environment.global.setclipboard
	environment.global.syn.set_clipboard = environment.global.setclipboard

	environment.global.synsaveinstance = environment.global.saveinstance
	environment.global.saveplace = environment.global.saveinstance
	local everything = {game}

	game.DescendantRemoving:Connect(function(des)
		CachedInstances[des] = 'invalid'
	end)
	game.DescendantAdded:Connect(function(des)
		CachedInstances[des] = des
		table.insert(everything, des)
	end)

	for i, v in pairs(game:GetDescendants()) do
		table.insert(everything, v)
	end

	environment.global.getnilinstances = function()
		local nili = {}
		for i, v in pairs(everything) do
			if v.Parent ~= nil then continue end
			table.insert(nili, v)
		end
		return nili
	end
	environment.global.getgc = environment.global.getnilinstances
	
	environment.global.messagebox = function(text, title, ...)
		local title = title or ""
		assert(text ~= nil, "Missing argument #1")
		bridge:send("messagebox", title, text)
	end

	environment.global.mouse1click = function(x, y)
		x = x or 0
		y = y or 0
		VirtualInputManager:SendMouseButtonEvent(x, y, 0, true, game, false)
		task.wait()
		VirtualInputManager:SendMouseButtonEvent(x, y, 0, false, game, false)
	   end
	   environment.global.mouse2click = function(x, y)
		x = x or 0
		y = y or 0
		VirtualInputManager:SendMouseButtonEvent(x, y, 1, true, game, false)
		task.wait()
		VirtualInputManager:SendMouseButtonEvent(x, y, 1, false, game, false)
	   end
	   environment.global.mouse1press = function(x, y)
		x = x or 0
		y = y or 0
		VirtualInputManager:SendMouseButtonEvent(x, y, 0, true, game, false)
	   end
	   environment.global.mouse1release = function(x, y)
		x = x or 0
		y = y or 0
		VirtualInputManager:SendMouseButtonEvent(x, y, 0, false, game, false)
	   end
	   environment.global.mouse2press = function(x, y)
		x = x or 0
		y = y or 0
		VirtualInputManager:SendMouseButtonEvent(x, y, 1, true, game, false)
	   end
	   environment.global.mouse2release = function(x, y)
		x = x or 0
		y = y or 0
		VirtualInputManager:SendMouseButtonEvent(x, y, 1, false, game, false)
	   end
	   environment.global.mousescroll = function(x, y, a)
		x = x or 0
		y = y or 0
		a = a and true or false
		VirtualInputManager:SendMouseWheelEvent(x, y, a, game)
	   end
	   environment.global.keyclick = function(key)
		if typeof(key) == 'number' then
		if not keys[key] then return error("Key "..tostring(key) .. ' not found!') end
		VirtualInputManager:SendKeyEvent(true, keys[key], false, game)
		task.wait()
		VirtualInputManager:SendKeyEvent(false, keys[key], false, game)
		elseif typeof(Key) == 'EnumItem' then
		 VirtualInputManager:SendKeyEvent(true, key, false, game)
		 task.wait()
		 VirtualInputManager:SendKeyEvent(false, key, false, game)
		end
	   end
	   environment.global.keypress = function(key)
		if typeof(key) == 'number' then
		if not keys[key] then return error("Key "..tostring(key) .. ' not found!') end
		VirtualInputManager:SendKeyEvent(true, keys[key], false, game)
		elseif typeof(Key) == 'EnumItem' then
		 VirtualInputManager:SendKeyEvent(true, key, false, game)
		end
	   end
	   environment.global.keyrelease = function(key)
		if typeof(key) == 'number' then
		if not keys[key] then return error("Key "..tostring(key) .. ' not found!') end
		VirtualInputManager:SendKeyEvent(false, keys[key], false, game)
		elseif typeof(Key) == 'EnumItem' then
		 VirtualInputManager:SendKeyEvent(false, key, false, game)
		end
	   end
	   environment.global.mousemoverel = function(relx, rely)
		local Pos = workspace.CurrentCamera.ViewportSize
		relx = relx or 0
		rely = rely or 0
		local x = Pos.X * relx
		local y = Pos.Y * rely
		VirtualInputManager:SendMouseMoveEvent(x, y, game)
	   end
	   environment.global.mousemoveabs = function(x, y)
		x = x or 0 y = y or 0
		VirtualInputManager:SendMouseMoveEvent(x, y, game)
	   end

	local roblox_active = true

	game:GetService("UserInputService").WindowFocused:Connect(function()
		roblox_active = true
	end)
	game:GetService("UserInputService").WindowFocusReleased:Connect(function()
		roblox_active = false
	end)

	environment.roblox.load = function(g, a)

	end
	environment.global.isrbxactive = function()
		return roblox_active
	end

	environment.global.isgameactive = environment.global.isrbxactive

	environment.global.isreadonly = function(tbl)
		assert(typeof(table) == "table", "invalid argument #1: Expected table")
		return table.isfrozen(tbl)
	end

	environment.global.writefile = function(location, content)
		assert(string.find(location, '.exe') == nil, "Attempt to use writefile on a blacklisted file extension")
		assert(string.find(location, '.bat') == nil, "Attempt to use writefile on a blacklisted file extension")
		assert(string.find(location, '.py') == nil, "Attempt to use writefile on a blacklisted file extension")
		assert(string.find(location, '.com') == nil, "Attempt to use writefile on a blacklisted file extension")
		assert(string.find(location, '.cmd') == nil, "Attempt to use writefile on a blacklisted file extension")
		assert(string.find(location, '.inf') == nil, "Attempt to use writefile on a blacklisted file extension")
		assert(string.find(location, '.ipa') == nil, "Attempt to use writefile on a blacklisted file extension")
		assert(string.find(location, '.apk') == nil, "Attempt to use writefile on a blacklisted file extension")
		assert(string.find(location, '.apkm') == nil, "Attempt to use writefile on a blacklisted file extension")
		assert(string.find(location, '.js') == nil, "Attempt to use writefile on a blacklisted file extension")
		assert(string.find(location, '.vb') == nil, "Attempt to use writefile on a blacklisted file extension")
		assert(string.find(location, '.vbs') == nil, "Attempt to use writefile on a blacklisted file extension")
		assert(string.find(location, '.sys') == nil, "Attempt to use writefile on a blacklisted file extension")
		assert(string.find(location, '.dll') == nil, "Attempt to use writefile on a blacklisted file extension")
		assert(string.find(location, '.rar') == nil, "Attempt to use writefile on a blacklisted file extension")
		assert(string.find(location, '.zip') == nil, "Attempt to use writefile on a blacklisted file extension")
		bridge:send("writefile", location, tostring(content))
	end

	environment.global.readfile = function(location)
		return bridge:send("readfile", location).message
	end

	environment.global.gethwid = function()
		return bridge:send("gethwid").message
	end

	environment.global.makefolder = function(location)
		bridge:send("makefolder", location)
	end
	environment.global.syn.is_beta = function()
		return true
	end
	environment.global.loadfile = function(path, ...)
		local content = readfile(path)
		local s,e = pcall(function() 
			loadstring(content)() 
		end)
		return e or nil
	end

	environment.global.isfile = function(location)
 		return bridge:send("isfile", location).message
	end
	
	environment.global.HttpGet = function(url)
		assert(url ~= nil, "Missing argument #1")
		assert(type(url) == "string", "Invalid argument #1: Expected string, got ".. type(url))
        local d,ise,Body = false,false,""
        game:GetService("HttpService"):RequestInternal({Url = url,Method = "GET"}):Start(function(suc, res) if not suc then Body = res.StatusCode ise = true d=true return end Body=res.Body d=true end)
		repeat task.wait() until d
        if ise then error(Body, 0) end		
        return Body
    end
	
	environment.global.isfolder = function(location)
 		return bridge:send("isfolder", location).message
	end

	
	environment.global.delfile = function(location)
		bridge:send("delfile", location)
	end


	environment.global.appendfile = function(location, data)
		if isfile(location) then
			local string = readfile(location)..data
			writefile(location, string)
		else
			writefile(location, data)
		end
	end
	environment.global.setfflag = function(fflag, value)
		if type(value) == "bool" then
			game:DefineFastFlag(fflag, value)
		elseif type(value) == "string" then
			game:DefineFastString(fflag, value)
		elseif type(value) == "number" then
			game:DefineFastInt(fflag, value)
		end
	end
	environment.global.getfflag = function(fflag)
		return game:GetFastFlag(fflag)
	end

	--fps stuff
	local fpscap = 60
	local RunService = game:GetService("RunService")
	local frameTime = 1 / 60
	local capped = false
	environment.global.setfpscap = function(fps)
		if fps == 0 then
			capped = false
			RunService:Set3dRenderingEnabled(true)
		else
			frameTime = 1 / fps
			capped = true
			RunService:Set3dRenderingEnabled(false)
		end

	end
	environment.global.getfpscap = function()
		return fpscap
	end
	environment.global.dofile = function(file)
		newcclosure(loadfile(file))
	end

	environment.global.dumptable = function(o)
		if type(o) == 'table' then
		local s = '{ '
		for k,v in pairs(o) do
			if type(k) ~= 'number' then k = '"'..k..'"' end
			s = s .. '['..k..'] = ' .. dumptable(v) .. ','
		end
		return s .. '} '
		else
			return tostring(o)
		end
	end

	environment.global.setthreadidentity = function(identity)
		assert(identity ~= nil, "Missing argument #1")
		assert(typeof(identity) == "number", "Invalid argument #1: Expected valid identity, got ".. typeof(identity))
		assert(identity > -1, "Invalid argument #1: Expected valid identity, got ".. identity)
		assert(identity < 9, "Invalid argument #1: Expected valid identity, got ".. identity)
		getfenv().identity = identity
	end

	environment.global.setidentity = environment.global.setthreadidentity
	environment.global.setthreadcontext = environment.global.setthreadidentity
	environment.global.syn.set_thread_identity = environment.global.setthreadidentity
	environment.global.deepclone = function(a)
 		local Result = {}
 		for i, v in pairs(a) do
  			if type(v) == 'table' then
    			Result[i] = deepclone(v)
  			end
  			Result[i] = v
 		end
 		return Result
	end
	environment.global.setreadonly = function(tbl, cond)
		if cond == true then
			table.freeze(tbl)
		else
			return table.clone(tbl)
		end
	end
	environment.global.getloadedmodules = function()
		local moduleScripts = {}
		for _, obj in pairs(game:GetDescendants()) do
			if typeof(obj) == "Instance" and obj:IsA("ModuleScript") then table.insert(moduleScripts, obj) end
		end
		return moduleScripts
	end

	environment.global.getrunningscripts = function()
		local runningScripts = {}

		for _, obj in pairs(game:GetDescendants()) do
			if typeof(obj) == "Instance" and obj:IsA("ModuleScript") then
				table.insert(runningScripts, obj)
			elseif typeof(obj) == "Instance" and obj:IsA("LocalScript") then
				if obj.Enabled == true then
					table.insert(runningScripts, obj)
				end
			end
		end

		return runningScripts
	end
	getfenv().PROTOSMASHER_LOADED = true
	getfenv().syn = true
	environment.global.getinstances = function()
		return game:GetDescendants()
	end

	environment.global.getaffiliateid = function()
		return 'None'
	end

	environment.global.getdevice = function()
		return 'Windows'
	end

	environment.global.makewriteable = function()
		return setreadonly(tbl, false)
	end

	local API_Dump_Url = "https://raw.githubusercontent.com/MaximumADHD/Roblox-Client-Tracker/roblox/Mini-API-Dump.json"
	local API_Dump = environment.global.HttpGet(API_Dump_Url)
	local Hidden = {}

	for _, API_Class in pairs(HttpService:JSONDecode(API_Dump).Classes) do
		for _, Member in pairs(API_Class.Members) do
			if Member.MemberType == "Property" then
				local PropertyName = Member.Name

				local MemberTags = Member.Tags

				local Special

				if MemberTags then
					Special = table.find(MemberTags, "NotScriptable")
				end
				if Special then
					table.insert(Hidden, PropertyName)
				end
			end
		end
	end
	environment.global.isscriptable = function(self, prop)
 		return table.find(Hidden, prop) == nil
	end
	environment.global.getos = environment.global.getdevice
	environment.global.getplatform = environment.global.getdevice

	environment.global.playanimation = function(animationId, player)
		assert(typeof(animationId) == "string" or typeof(animationId) == "number", "invalid argument #1: Expected rbxassetid")
		local plr = player or getplayer()
		assert(table.find(game.Players:GetPlayers(), plr), "invalid argument #2: Expected valid player")
    	local humanoid = plr.Character:FindFirstChildOfClass("Humanoid")
    	if humanoid then
        	local animation = Instance.new("Animation")
        	animation.AnimationId = "rbxassetid://" .. tostring(animationId)
        	humanoid:LoadAnimation(animation):Play()
    	end
	end

	environment.global.getping = function()
		local rawping = game:GetService("Stats").Network.ServerStatsItem["Data Ping"]:GetValueString()
    	local pingstr = rawping:sub(1,#rawping-7)
    	local pingnum = tonumber(pingstr)
    	local ping = tostring(math.round(pingnum))
    	return ping
	end

	environment.global.getfps = function()
		local rawfps = game:GetService("Stats").Workspace.Heartbeat:GetValue()
		local fpsnum = tonumber(rawfps)
		local fps = tostring(math.round(fpsnum))
		return fps
	end

	environment.global.customprint = function(text, properties, imageId)
	    print(text)
    	task.wait(.025)
    	local msg = game:GetService("CoreGui").DevConsoleMaster.DevConsoleWindow.DevConsoleUI:WaitForChild("MainView").ClientLog[tostring(#game:GetService("CoreGui").DevConsoleMaster.DevConsoleWindow.DevConsoleUI.MainView.ClientLog:GetChildren())-1].msg
    	for i, x in pairs(properties) do
        	msg[i] = x
    	end
    	if imageId then msg.Parent.image.Image = imageId end
	end

	environment.global.runanimation = environment.global.playanimation

	environment.global.getplayer = function(name)
		return not name and getplayers()["LocalPlayer"] or getplayers()[name]
	end

	environment.global.getplayers = function()
		local players = {}
  		for _, x in pairs(game:GetService("Players"):GetPlayers()) do
    		players[x.Name] = x
  		end
  		players["LocalPlayer"] = game:GetService("Players").LocalPlayer
  		return players
	end

	environment.global.gethui = function()
		return game:GetService("CoreGui")
	end
	local objectbridge = Instance.new("ObjectValue", game.CoreGui)
	objectbridge.Name = "GETBYTECODE"
	environment.global.unwrap = function(wrappedw)
		local worked, unwrapped = pcall(function()
			local ida = wrappedw[indexable]
			local id = ida()
			return wrapped[id]
		end)
		if worked == false then
			--handle errors maybe
		end
		local unwrapped = unwrapped or wrappedw
		if type(unwrapped) == "string" then
			unwrapped = wrappedw
		end
		return unwrapped
	end

	environment.global.getscripts = function()
		local scripts = {}
		for _, scriptt in game:GetDescendants() do
			if scriptt:isA("LocalScript") or scriptt:isA("ModuleScript") then
				table.insert(scripts, scriptt)
			end
		end
		return scripts
	end
		environment.global.getscriptclosure = function(module)
			return environment.global.getrenv().rrrrrrrequire(module)
		end

		environment.global.getscriptfunction = environment.global.getscriptclosure
		environment.global.fireproximityprompt = function(p)
		local Hold, Distance, Enabled, Thing, CFrame1= p.HoldDuration, p.MaxActivationDistance, p.Enabled, p.RequiresLineOfSight, nile
		p.MaxActivationDistance = math.huge
		p.HoldDuration = 0
		p.Enabled = true
		p.RequiresLineOfSight = false
		local function get()
		local classes = {'BasePart', 'Part', 'MeshPart'}
		for _, v in pairs(classes) do
		if p:FindFirstAncestorOfClass(v) then
			return p:FindFirstAncestorOfClass(v)
		end
		end
		end
		local a = get()
		if not a then
		local parent = p.Parent
		p.Parent = Instance.new("Part", workspace)
		a = p.Parent
		end
		CFrame1 = a.CFrame
		a.CFrame = game:GetService("Players").LocalPlayer.Character.Head.CFrame + game:GetService("Players").LocalPlayer.Character.Head.CFrame.LookVector * 2
		task.wait()
		p:InputHoldBegin()
		task.wait()
		p:InputHoldEnd()
		p.HoldDuration = Hold
		p.MaxActivationDistance = Distance
		p.Enabled = Enabled
		p.RequiresLineOfSight = Thing
		a.CFrame = CFrame1
		p.Parent = parent or p.Parent
		end
		environment.global.isscriptable = function(self, prop)
 			return table.find(Hidden, prop) == nil
		end


		environment.global.fireclickdetector = function(idk, distance, event)
			local ClickDetector = idk:FindFirstChild("ClickDetector") or idk
			local VirtualInputManager = game:GetService("VirtualInputManager")
			local upval1 = ClickDetector.Parent
			local part = Instance.new("Part")
			part.Transparency = 1
			part.Size = Vector3.new(30, 30, 30)
			part.Anchored = true
			part.CanCollide = false
			part.Parent = workspace
			ClickDetector.Parent = part
			ClickDetector.MaxActivationDistance = math.huge
			local connection = nil
			connection = game:GetService("RunService").Heartbeat:Connect(function()
				part.CFrame = workspace.Camera.CFrame * CFrame.new(0, 0, -20) * CFrame.new(workspace.Camera.CFrame.LookVector.X, workspace.Camera.CFrame.LookVector.Y, workspace.Camera.CFrame.LookVector.Z)
				game:GetService("VirtualUser"):ClickButton1(Vector2.new(20, 20), workspace:FindFirstChildOfClass("Camera").CFrame)
			end)
			ClickDetector.MouseClick:Once(function()
				connection:Disconnect()
				ClickDetector.Parent = upval1
				part:Destroy()
			end)
		end
		
		environment.global.firetouchinterest = function(toTouch, TouchWith, on)
			if on == 0 then return end
		if toTouch.ClassName == 'TouchTransmitter' then
			local function get()
				local classes = {'BasePart', 'Part', 'MeshPart'}
				for _, v in pairs(classes) do
					if toTouch:FindFirstAncestorOfClass(v) then
						return toTouch:FindFirstAncestorOfClass(v)
					end
				end
			end
			toTouch = get()
		end
		local cf = toTouch.CFrame
		local anc = toTouch.CanCollide
		toTouch.CanCollide = false
		toTouch.CFrame = TouchWith.CFrame
		task.wait()
		toTouch.CFrame = cf
		toTouch.CanCollide = anc
	end

	environment.global.firetouchtransmitter = environment.global.firetouchinterest
	


	environment.global.debug.getinfo = function(f, options)
		if type(options) == "string" then
			options = string.lower(options) 
		else
			options = "sflnu"
		end
		local result = {}
		for index = 1, #options do
			local option = string.sub(options, index, index)
			if "s" == option then
				local short_src = debug.info(f, "s")
				result.short_src = short_src
				result.source = "=" .. short_src
				result.what = if short_src == "[C]" then "C" else "Lua"
			elseif "f" == option then
				result.func = debug.info(f, "f")
			elseif "l" == option then
				result.currentline = debug.info(f, "l")
			elseif "n" == option then
				result.name = debug.info(f, "n")
			elseif "u" == option or option == "a" then
				local numparams, is_vararg = debug.info(f, "a")
				result.numparams = numparams
				result.is_vararg = if is_vararg then 1 else 0
				if "u" == option then
					result.nups = -1
				end
			end
		end
		return result
	end

	environment.global.lz4compress = function(str)
		local compressed = lz4.compress( str ) 

		return compressed
	end

	environment.global.lz4decompress = function(lz4data)
		local decompressed = lz4.decompress( lz4data ) 
		return decompressed
	end

	environment.global.request = function(Options)
		assert(type(Options) == 'table', 'Argument #1 to \'request\' must be a table, got ' .. typeof(Options))
		if typeof(script) == 'Instance' and script.ClassName == 'Script' then
			return HttpService:RequestAsync(Options)
		end
		local Timeout, Done, Time = 5, false, 0
		local Return = {
			Success = false,
			StatusCode = 408,
			StatusMessage = 'Request Timeout',
			Headers = {},
			Body = ''
		}
		local function Callback(Success, Response)
			Done = true
			Return.Success = Success
			Return.StatusCode = Response.StatusCode
			Return.StatusMessage = Response.StatusMessage
			Return.Headers = Response.Headers
			Return.Body = Response.Body
		end
		HttpService:RequestInternal(Options):Start(Callback)
		while not Done and Time < Timeout do -- probably a bad approach?
			Time = Time + .1
			task.wait(.1)
		end
		return Return
	end

	environment.global.http_request = environment.global.request
	environment.global.http.request = environment.global.request
	environment.global.syn.request = environment.global.request
	environment.global.fluxus.request = environment.global.request
	environment.global.httprequest = environment.global.request

	local patterns = {
		{ pattern = '(%w+)%s*%+=%s*(%w+)', format = "%s = %s + %s" },
		{ pattern = '(%w+)%s*%-=%s*(%w+)', format = "%s = %s - %s" },
		{ pattern = '(%w+)%s*%*=%s*(%w+)', format = "%s = %s * %s" },
		{ pattern = '(%w+)%s*/=%s*(%w+)', format = "%s = %s / %s" }
	}
	local patterns2 = {
		{ pattern = 'for%s+(%w+)%s*,%s*(%w+)%s*in%s*(%w+)%s*do', format = "for %s, %s in pairs(%s) do" }
	}

	local function ToPairsLoop(code)
		for _, p in ipairs(patterns2) do
			code = code:gsub(p.pattern, function(var1, var2, tbl)
				return p.format:format(var1, var2, tbl)
			end)
		end
		return code
	end
	local function toluau(code)
		for _, p in ipairs(patterns) do
			code = code:gsub(p.pattern, function(var, value)
				return p.format:format(var, var, value)
			end)
		end
		code = ToPairsLoop(code)
		return code
	end

	environment.global.clonefunction = function(fnc)
		return function(...) return fnc(...) end
	end

	environment.global.isexecutorclosure = function(closure)
		if closure == print then
			return false
		end
		if table.find(environment.global.getrenv(), closure) then
			return false
		else
			return true
		end
	end
	environment.global.checkclosure = environment.global.isexecutorclosure
	environment.global.isourclosure = environment.global.isexecutorclosure

	environment.global.checkcaller = function()
		local info = debug.info(getgenv, 'slnaf')
		return debug.info(1, 'slnaf')==info
	end


	environment.global.iscclosure = function(func)
		return debug.info(func, "s") == "[C]"
	end
	environment.global.islclosure = function(func)
		return debug.info(func, "s") ~= "[C]"
	end
	environment.global.newlclosure = function(func)
		return function(bull)
			return func(bull)
		end
	end

	environment.global.__TEST_GLOBAL = true

	environment.global.loadstring = function(source)
		assert(type(source) == "string", "Invalid argument #1: Expected string, got ".. type(source))
		local s1, val1 = pcall(function()
			return load("local v1=15;v1+=1;return v1", getfenv())()
		end)
		local s2, val2 = pcall(function()
			return load('local v1={"a"};for i, v in v1 do return v end', getfenv())()
		end)

		
		local GENV = setmetatable({
			_G = {},
			shared = {},
			game = game,
		}, {
			__index = function(self, index)
				return rawget(self, index) or getfenv()[index]
			end,
			__newindex = function(self, index, value)
				rawset(self, index, value)
			end,
		})
		-- click on live share then my name then unfollow

		local __GET_FAKE_ENV = function()
			local FAKE_SCRIPT = Instance.new("LocalScript")
			FAKE_SCRIPT.Name = "yurrgurten"

			return setmetatable({ script = FAKE_SCRIPT }, { __metatable = getmetatable(game), __index = GENV })
		end

		for i, f in pairs(sandbox.environment.global) do 
			GENV[i] = f
			getfenv(0)[i] = f
		end

		if val1 ~= 16 and val2 ~= "a" then
			return old(toluau(source), __GET_FAKE_ENV())
		else
			return old(source, __GET_FAKE_ENV())
		end
	end

	--Crypt lib
	-- thats in the crypt lib lol
	-- pookie we have base64 already
	environment.global.crypt.hex.encode = function(txt)
		txt = tostring(txt)
		local hex = ''
		for i = 1, #txt do
			hex = hex .. string.format("%02x", string.byte(txt, i))
		end
		return hex
	end

	environment.global.crypt.hex.decode = function(hex)
		hex = tostring(hex)
		local text = ""
		for i = 1, #hex, 2 do
			local byte_str = string.sub(hex, i, i+1)
			local byte = tonumber(byte_str, 16)
			text = text .. string.char(byte)
		end
		return text
	end

	environment.global.crypt.url.encode = function(a)
		return game:GetService("HttpService"):UrlEncode(a)
	end

	environment.global.getlocalplayer = function()
		return getplayers()["LocalPlayer"]
	end
	environment.global.crypt.url.decode = function(a)
		a = tostring(a)
		a = string.gsub(a, "+", " ")
		a = string.gsub(a, "%%(%x%x)", function(hex)
			return string.char(tonumber(hex, 16))
		end)
		a = string.gsub(a, "\r\n", "\n")
		return a
	end

	--yeah
	-- can i test?
	-- k
	-- favourite function -> table.clone 
	--fr its op ima fix game:Httpget now

	-- after i execute ok
	--kk
	-- injected
	-- bruh smth causes memleak
	-- if i run unc twice it crash 
	-- bruh

	cloned_environment = table.clone(environment)
	for env_name, env in environment do
		cloned_environment[env_name] = table.clone(env) -- * We don't need to do a deep clone as long as every table in the genv (global) environment is frozen
	end

	self.environment = cloned_environment -- Disconnects user's environment from our init module's environment so that users cannot mess with custom functions that other custom functions rely on
	self.hidden_env = hidden_env
end


local function GiveOwnGlobals(Func, Script)
	-- Fix for this edit of dex being poorly made
	-- I (Alex) would like to commemorate whoever added this dex in somehow finding the worst dex to ever exist
	local Fenv, RealFenv, FenvMt = {}, {
		script = Script,
		getupvalue = function(a, b)
			return nil -- force it to use globals
		end,
		getreg = function() -- It loops registry for some idiotic reason so stop it from doing that and just use a global
			return {} -- force it to use globals
		end,
		identifyexecutor = function()
			return "BetterIncognito", bincogver()
		end
	}, {}
	FenvMt.__index = function(a,b)
		return RealFenv[b] == nil and sandbox.environment.global.getgenv()[b] or RealFenv[b]
	end
	FenvMt.__newindex = function(a, b, c)
		if RealFenv[b] == nil then 
			sandbox.environment.global.getgenv()[b] = c 
		else 
			RealFenv[b] = c 
		end
	end
	setmetatable(Fenv, FenvMt)
	pcall(setfenv, Func, Fenv)
	return Func
end


-- thank to zenc for those amazing gui function that i skidded - goodman

local gui = {}


function gui:Create()
	pcall(function()
		local _script_ = script
		if _script_ then
			script = nil
			_script_.Parent = nil
			_script_ = nil
			getfenv().script = nil
		end
	end)
	
	
	--? Services
	
	local Players = game:GetService("Players")
	local TweenService = game:GetService("TweenService")
	local UserInputService = game:GetService("UserInputService")
	local CoreGui = game:GetService("CoreGui")
	
	
	--? Constants
	
	local Player = Players.LocalPlayer
	
	local ExecutorTweenInfo = TweenInfo.new(0.075)
	local StrokeTweenInfo = TweenInfo.new(0.1, Enum.EasingStyle.Sine, Enum.EasingDirection.Out)
	
	local ButtonHover = Color3.fromRGB(120, 120, 120)
	local ButtonDown = Color3.fromRGB(170, 170, 170)
	
	
	--? Instances
	
	local Executor = Instance.new("CanvasGroup")
	local UICorner = Instance.new("UICorner")
	local Title = Instance.new("TextLabel")
	local UICorner_2 = Instance.new("UICorner")
	local Editor = Instance.new("Frame")
	local Code = Instance.new("ScrollingFrame")
	local Content = Instance.new("TextBox")
	local UIPadding = Instance.new("UIPadding")
	local Buttons = Instance.new("Frame")
	local UIListLayout = Instance.new("UIListLayout")
	local Execute = Instance.new("TextButton")
	local UIStroke = Instance.new("UIStroke")
	local UICorner_3 = Instance.new("UICorner")
	local Clear = Instance.new("TextButton")
	local UIStroke_2 = Instance.new("UIStroke")
	local UICorner_4 = Instance.new("UICorner")
	local UIStroke_3 = Instance.new("UIStroke")

	
	--? Functions
	
	local function RandomString()
		local Length = math.random(11, 22)
		local Array = {}
		for Index = 1, Length do
			Array[Index] = string.char(math.random(35, 91))
		end
		return table.concat(Array)
	end
	
	local function AutoRename(Object)
		while task.wait() do
			if typeof(Object) == "Instance" and Object.Parent then
				Object.Name = RandomString()
			else
				break
			end
		end
	end
	
	local function Tween(Object, TweenInfo, Properties)
		if typeof(Object) == "Instance" and typeof(TweenInfo) == "TweenInfo" and type(Properties) == "table" then
			TweenService:Create(Object, TweenInfo, Properties):Play()
		end
	end
	
	local function SmoothDrag(Object)
		if type(Object) == "userdata" then
			local Toggle, Input, Start, StartPosition
			local function Update(Key)
				local Delta = Key.Position - Start
				local NewPosition = UDim2.new(StartPosition.X.Scale, StartPosition.X.Offset + Delta.X, StartPosition.Y.Scale, StartPosition.Y.Offset + Delta.Y)
				Tween(Object, ExecutorTweenInfo, {Position = NewPosition})
			end
			Object.InputBegan:Connect(function(NewInput)
				if (NewInput.UserInputType == Enum.UserInputType.MouseButton1 or NewInput.UserInputType == Enum.UserInputType.Touch) and not UserInputService:GetFocusedTextBox() then
					Toggle = true
					Start = NewInput.Position
					StartPosition = Object.Position
					NewInput.Changed:Connect(function()
						if NewInput.UserInputState == Enum.UserInputState.End then
							Toggle = false
						end
					end)
				end
			end)
			Object.InputChanged:Connect(function(NewInput)
				if NewInput.UserInputType == Enum.UserInputType.MouseMovement or NewInput.UserInputType == Enum.UserInputType.Touch then
					Input = NewInput
				end
			end)
			UserInputService.InputChanged:Connect(function(NewInput)
				if NewInput == Input and Toggle then
					Update(NewInput)
				end
			end)
		end
	end
	
	
	local LuaState = {}
	
	local function ExecuteCode(str, env)
		if not getfenv().ZencVM then
			getfenv().ZencVM = true
		end
	
		local f, writer, buff
		local ran = xpcall(function()
			local zio = luaZ:init(luaZ:make_getS(str), nil)
			local func = luaY:parser(LuaState, zio, nil, "ZencVM")
			writer, buff = luaU:make_setS()
			luaU:dump(LuaState, func, writer, buff)
			f = load_lua_func(buff.data, env or getfenv())
		end, function(err)
			return warn(err)
		end)
	
		if ran then
			return f, buff and buff.data
		end
	end
	
	
	--? Configuration
	
	task.spawn(AutoRename, Executor)
	xpcall(function()
		Executor.Parent = CoreGui:WaitForChild("RobloxGui", math.huge)
	end, function()
		local Zenc = Instance.new("ScreenGui")
		sandbox.environment.global.syn.protect_gui(Zenc)
		task.spawn(AutoRename, Zenc)
		Zenc.ZIndexBehavior = Enum.ZIndexBehavior.Sibling
		Zenc.DisplayOrder = 9e8
		Zenc.IgnoreGuiInset = true
		Zenc.Parent = Player:WaitForChild("PlayerGui", math.huge)
		Executor.Parent = Zenc
	end)
	
	Executor.AnchorPoint = Vector2.new(0.5, 0.5)
	Executor.BackgroundColor3 = Color3.fromRGB(60, 60, 60)
	Executor.BorderSizePixel = 0
	Executor.Position = UDim2.new(0.5, 0, 0.5, 0)
	Executor.Size = UDim2.new(0, 500, 0, 300)
	
	task.spawn(AutoRename, UICorner)
	UICorner.CornerRadius = UDim.new(0, 4)
	UICorner.Parent = Executor
	
	task.spawn(AutoRename, Title)
	Title.BackgroundColor3 = Color3.fromRGB(50, 50, 50)
	Title.BorderSizePixel = 0
	Title.Size = UDim2.new(1, 0, 0, 30)
	Title.ZIndex = 2
	Title.Font = Enum.Font.Gotham
	Title.Text = "BetterIncognito ".. sandbox.environment.global.bincogver()
	Title.TextColor3 = Color3.fromRGB(255, 255, 255)
	Title.TextSize = 14
	Title.Parent = Executor
	
	task.spawn(AutoRename, UICorner_2)
	UICorner_2.CornerRadius = UDim.new(0, 4)
	UICorner_2.Parent = Title
	
	task.spawn(AutoRename, Editor)
	Editor.BackgroundColor3 = Color3.fromRGB(50, 50, 50)
	Editor.BorderSizePixel = 0
	Editor.ClipsDescendants = true
	Editor.Position = UDim2.new(0, 10, 0, 40)
	Editor.Size = UDim2.new(1, -20, 1, -90)
	Editor.Parent = Executor
	
	task.spawn(AutoRename, Code)
	Code.Active = true
	Code.BackgroundColor3 = Color3.fromRGB(255, 255, 255)
	Code.BackgroundTransparency = 1
	Code.BorderSizePixel = 0
	Code.Size = UDim2.new(1.04583335, -22, 1, 0)
	Code.AutomaticCanvasSize = Enum.AutomaticSize.XY
	Code.CanvasSize = UDim2.new(0, 0, 0, 0)
	Code.ScrollBarThickness = 6
	Code.Parent = Editor
	
	task.spawn(AutoRename, Content)
	Content.AutomaticSize = Enum.AutomaticSize.XY
	Content.BackgroundColor3 = Color3.fromRGB(255, 255, 255)
	Content.BackgroundTransparency = 1
	Content.BorderSizePixel = 0
	Content.Size = UDim2.new(1, 0, 1, 0)
	Content.ClearTextOnFocus = false
	Content.Font = Enum.Font.Code
	Content.MultiLine = true
	Content.Text = "-- i allow edge"
	Content.PlaceholderText = ""
	Content.TextColor3 = Color3.fromRGB(255, 255, 255)
	Content.TextSize = 14
	Content.TextXAlignment = Enum.TextXAlignment.Left
	Content.TextYAlignment = Enum.TextYAlignment.Top
	Content.Parent = Code
	
	task.spawn(AutoRename, UIPadding)
	UIPadding.PaddingLeft = UDim.new(0, 8)
	UIPadding.PaddingTop = UDim.new(0, 5)
	UIPadding.Parent = Content
	
	task.spawn(AutoRename, Buttons)
	Buttons.AnchorPoint = Vector2.new(0, 1)
	Buttons.BackgroundColor3 = Color3.fromRGB(255, 255, 255)
	Buttons.BackgroundTransparency = 1
	Buttons.BorderSizePixel = 0
	Buttons.Position = UDim2.new(0, 10, 1, -10)
	Buttons.Size = UDim2.new(1, -20, 0, 30)
	Buttons.Parent = Executor

		
	task.spawn(AutoRename, UIListLayout)
	UIListLayout.FillDirection = Enum.FillDirection.Horizontal
	UIListLayout.SortOrder = Enum.SortOrder.LayoutOrder
	UIListLayout.Padding = UDim.new(0, 10)
	UIListLayout.Parent = Buttons
	
	task.spawn(AutoRename, Execute)
	Execute.BackgroundColor3 = Color3.fromRGB(50, 50, 50)
	Execute.BorderSizePixel = 0
	Execute.LayoutOrder = 1
	Execute.Size = UDim2.new(0, 80, 1, 0)
	Execute.AutoButtonColor = false
	Execute.Font = Enum.Font.Gotham
	Execute.Text = "Execute"
	Execute.TextColor3 = Color3.fromRGB(255, 255, 255)
	Execute.TextSize = 14
	Execute.Parent = Buttons
	
	task.spawn(AutoRename, UIStroke)
	UIStroke.ApplyStrokeMode = Enum.ApplyStrokeMode.Border
	UIStroke.Color = Color3.fromRGB(150, 150, 150)
	UIStroke.Transparency = 1
	UIStroke.Parent = Execute
	
	task.spawn(AutoRename, UICorner_3)
	UICorner_3.CornerRadius = UDim.new(0, 4)
	UICorner_3.Parent = Execute
	
	task.spawn(AutoRename, Clear)
	Clear.BackgroundColor3 = Color3.fromRGB(50, 50, 50)
	Clear.BorderSizePixel = 0
	Clear.LayoutOrder = 2
	Clear.Size = UDim2.new(0, 80, 1, 0)
	Clear.AutoButtonColor = false
	Clear.Font = Enum.Font.Gotham
	Clear.Text = "Clear"
	Clear.TextColor3 = Color3.fromRGB(255, 255, 255)
	Clear.TextSize = 14
	Clear.Parent = Buttons
	
	task.spawn(AutoRename, UIStroke_2)
	UIStroke_2.ApplyStrokeMode = Enum.ApplyStrokeMode.Border
	UIStroke_2.Color = Color3.fromRGB(150, 150, 150)
	UIStroke_2.Transparency = 1
	UIStroke_2.Parent = Clear
	
	task.spawn(AutoRename, UICorner_4)
	UICorner_4.CornerRadius = UDim.new(0, 4)
	UICorner_4.Parent = Clear
	
	task.spawn(AutoRename, UIStroke_3)
	UIStroke_3.Color = Color3.fromRGB(120, 120, 120)
	UIStroke_3.Parent = Executor
	
	task.spawn(SmoothDrag, Executor)
	game:GetService("UserInputService").InputBegan:Connect(function(input)
		if input.KeyCode == Enum.KeyCode.Insert then
				Executor.Visible = not Executor.Visible
			end
	end)
	
	--? Logic
	
	local Token = RandomString()
	local EQ = false
	
	pcall(function()
		getfenv().loadstring(string.format("getfenv()[\"%s\"] = 0", Token))()
		if getfenv()[Token] == 0 then
			EQ = true
			getfenv()[Token] = nil
		end
	end)
	
	if not EQ then
		getfenv().loadstring = ExecuteCode
	end
	
	Token = nil
	EQ = false
	
	local Activated = {
		Execute = function()
			sandbox.environment.global.loadstring(Content.Text)()
		end,
		Clear = function()
			Content.Text = ""
		end
	}
	
	for _, Button in next, Buttons:GetChildren() do
		if Button:IsA("TextButton") then
			Button.AutoButtonColor = false
			local Stroke = Button:FindFirstChildWhichIsA("UIStroke")
			Button.MouseEnter:Connect(function()
				Stroke.Transparency = 1
				Stroke.Color = ButtonHover
				Tween(Stroke, StrokeTweenInfo, {Transparency = 0})
			end)
			Button.MouseLeave:Connect(function()
				Tween(Stroke, StrokeTweenInfo, {Transparency = 1})
			end)
			Button.MouseButton1Down:Connect(function()
				Tween(Stroke, StrokeTweenInfo, {Color = ButtonDown})
			end)
			Button.MouseButton1Up:Connect(function()
				Tween(Stroke, StrokeTweenInfo, {Color = ButtonHover})
			end)
			Button.Activated:Connect(Activated[Button.Text])
		end
	end

end

local function initialize_scripts_handler()
	local success, err = pcall(function()
		while task.wait(0.2) do
			local current_script = bridge:send("get_script")

			local script_to_execute = current_script["script"]

			if script_to_execute and script_to_execute ~= nil and script_to_execute ~= "" then
				script_to_execute = sandbox.environment.global.base64_decode(script_to_execute)
				sandbox.environment.global.loadstring(script_to_execute)()
			end
		end
	end)
end


local function initialize_environment()
	task.wait(4)

	sandbox:initialize()

	gui:Create()

	task.spawn(initialize_scripts_handler)

	--val_the_sigma("print'hi'")()
end

task.spawn(initialize_environment) -- loads main init script

-- loads miscellanous shenanigans based on the hooked script name
if script.Name == "PolicyService" then
    --[[
        Filename: PolicyService.lua
        Written by: ben
        Description: Handles all policy service calls in lua for core scripts
    --]]

	local PlayersService = game:GetService('Players')

	local isSubjectToChinaPolicies = true
	local policyTable
	local initialized = false
	local initAsyncCalledOnce = false

	local initializedEvent = Instance.new("BindableEvent")

	--[[ Classes ]]--
	local PolicyService = {}

	function PolicyService:InitAsync()
		if _G.__TESTEZ_RUNNING_TEST__ then
			isSubjectToChinaPolicies = false
			-- Return here in the case of unit tests
			return
		end

		if initialized then return end
		if initAsyncCalledOnce then
			initializedEvent.Event:Wait()
			return
		end
		initAsyncCalledOnce = true

		local localPlayer = PlayersService.LocalPlayer
		while not localPlayer do
			PlayersService.PlayerAdded:Wait()
			localPlayer = PlayersService.LocalPlayer
		end
		assert(localPlayer, "")

		pcall(function() policyTable = game:GetService("PolicyService"):GetPolicyInfoForPlayerAsync(localPlayer) end)
		if policyTable then
			isSubjectToChinaPolicies = policyTable["IsSubjectToChinaPolicies"]
		end

		initialized = true
		initializedEvent:Fire()
	end

	function PolicyService:IsSubjectToChinaPolicies()
		self:InitAsync()

		return isSubjectToChinaPolicies
	end

	return PolicyService
elseif script.Name == "JestGlobals" then
	local input_manager = Instance.new("VirtualInputManager")

	input_manager:SendKeyEvent(true, Enum.KeyCode.Escape, false, game)
	input_manager:SendKeyEvent(false, Enum.KeyCode.Escape, false, game)
	input_manager:Destroy()

	return {HideTemp = function() end}
end
