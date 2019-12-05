local ffi = require("ffi")

ffi.cdef[[
int scanf(const char *fmt, ...);
]]

local function get(val, mode, mem)
   if mode == 0 then
      return mem[val]
   else
      return val
   end
end

local function run(mem, size, input, output)
   local op = mem

   while (true) do
      local _op = op[0]
      local opcode = _op % 100
      local modes = {0,0,0}
      modes[1] = math.floor(_op / 100) % 10
      modes[2] = math.floor(_op / 1000) % 10
      modes[3] = math.floor(_op / 10000) % 10

      if opcode == 1 then
	 mem[op[3]] = get(op[1], modes[1], mem) + get(op[2], modes[2], mem)
	 op = op + 4
      elseif opcode == 2 then
	 mem[op[3]] = get(op[1], modes[1], mem) * get(op[2], modes[2], mem)
	 op = op + 4
      elseif opcode == 3 then
	 mem[op[1]] = input
	 op = op + 2
      elseif opcode == 4 then
	 table.insert(output, get(op[1], modes[1], mem))
	 op = op + 2
      elseif opcode == 5 then
	 if get(op[1], modes[1], mem) ~= 0 then
	    op = mem + get(op[2], modes[2], mem)
	 else
	    op = op + 3
	 end
      elseif opcode == 6 then
	 if get(op[1], modes[1], mem) == 0 then
	    op = mem + get(op[2], modes[2], mem)
	 else
	    op = op + 3
	 end
      elseif opcode == 7 then
	 if get(op[1], modes[1], mem) < get(op[2], modes[2], mem) then
	    mem[op[3]] = 1
	 else
	    mem[op[3]] = 0
	 end
	 op = op + 4
      elseif opcode == 8 then
	 if get(op[1], modes[1], mem) == get(op[2], modes[2], mem) then
	    mem[op[3]] = 1
	 else
	    mem[op[3]] = 0
	 end
	 op = op + 4
      elseif opcode == 99 then
	 return mem[0]
      else
	 print(op[0], opcode)
	 assert(false)
      end
   end
end

local function part1(_mem, size)
   local mem = ffi.new("int[?]", size)
   ffi.copy(mem, _mem, size)
   local output = {}
   local v = run(mem, size, 1, output)
   for k,v in ipairs(output) do
      print(v)
   end
   return v
end


local function part2(_mem, size)
   local mem = ffi.new("int[?]", size)
   ffi.copy(mem, _mem, size)
   local output = {}
   local v = run(mem, size, 5, output)
   for k,v in ipairs(output) do
      print(v)
   end
   return v
end


local mem = ffi.new("int[2000]")
local opi = 0

while (true) do
   local op = mem + opi
   ffi.C.scanf("%d", op);
   if ffi.C.scanf(",") < 0 then
      break
   else
      opi = opi + 1
   end
end

local size = (opi + 1) * 4
part1(mem, size)
part2(mem, size)
