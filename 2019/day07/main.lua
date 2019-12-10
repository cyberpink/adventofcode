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

local function send(cs, id, msg)
   local e = cs[id].e + 1
   cs[id].q[e] = msg
   cs[id].e = e
end

local function recv(cs, id)
   local s = cs[id].s
   assert(s <= cs[id].e, "reading from empty channel")
   local msg = cs[id].q[s]
   cs[id].q[s] = nil
   cs[id].s = s + 1
   return msg
end

local function run_threads(t, inits)
   local pid = 1
   local channels = {}
   for i=1,#t,1 do
      channels[i] = { s = 1, e = 0, q = {}}
      send(channels, i, inits[i])
   end
   send(channels, 1, 0)
   ::next_thread::
   local _, v = coroutine.resume(t[pid])
   ::continue::
   if v.status == 0 then
      _, v = coroutine.resume(t[pid], recv(channels, pid))
      goto continue
   elseif v.status == 1 then
      send(channels, (pid%5)+1, v.value)
      pid = (pid%5) + 1
      goto next_thread
   elseif v.status == 2 then
      if pid == 5 then
         return recv(channels, 1)
      else
         pid = pid + 1
         goto next_thread
      end
   else
      print(v)
      assert(false)
   end
end

local function copy(xs)
   local out = {}
   for i=1, #xs, 1 do
      out[i] = xs[i]
   end
   return out
end

local function permutations(xs, ptr, out)
   if ptr == #xs then
      table.insert(out, copy(xs))
   else
      for i = ptr,#xs,1 do
         xs[ptr], xs[i] = xs[i], xs[ptr]
         permutations(xs, ptr+1, out)
         xs[ptr], xs[i] = xs[i], xs[ptr]
      end
   end
end

local function make_program()
   local amps = {}
   for i=1,5,1 do
      amps[i] = coroutine.create(function () make_vm(mem, size) end)
   end
   return amps
end

local function run_permutations(inputs)
   local ps = {}
   permutations(inputs, 1, ps)
   local max = 0
   for i=1,#ps,1 do
      local out = run_threads(make_program(), ps[i])
      if out > max then
         max = out
      end
   end
   return max
end

print(run_permutations({0,1,2,3,4}))
print(run_permutations({5,6,7,8,9}))
