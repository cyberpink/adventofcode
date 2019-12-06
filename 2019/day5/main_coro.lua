local ffi = require("ffi")

ffi.cdef[[
int scanf(const char *fmt, ...);
]]

local function get_msg()
   return coroutine.yield({ status = 0 })
end
local function put_msg(m)
   return coroutine.yield({ status = 1,  value = m })
end
local function thread_done()
   return coroutine.yield({ status = 2 })
end

local function run_thread(t, input)
   local _, v = coroutine.resume(t)
   while (true) do
      if v.status == 0 then
	 _, v = coroutine.resume(t, input)
      elseif v.status == 1 then
	 print(v.value)
	 _, v = coroutine.resume(t)
      elseif v.status == 2 then
	 break
      else
	 assert(false)
      end      
   end
end

local function get(val, mode, mem)
   if mode == 0 then
      return mem[val]
   else
      return val
   end
end

local function run(mem)
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
	 mem[op[1]] = get_msg()
	 op = op + 2
      elseif opcode == 4 then
	 put_msg(get(op[1], modes[1], mem))
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
	 thread_done()
      else
	 print(op[0], opcode)
	 assert(false)
      end
   end
end

local function make_vm(_mem, size)
   local mem = ffi.new("int[?]", size)
   ffi.copy(mem, _mem, size)
   local output = {}
   run(mem)
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
local part1 = coroutine.create(function () make_vm(mem, size) end)
local part2 = coroutine.create(function () make_vm(mem, size) end)

run_thread(part1, 1)
run_thread(part2, 5)
