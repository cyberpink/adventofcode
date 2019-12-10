local ffi = require("ffi")

ffi.cdef[[
int scanf(const char *fmt, ...);
]]

local function run(ops, size, n, v)
   ops[1] = n
   ops[2] = v

   local op = ops

   while (true) do
      if op[0] == 1 then
	 ops[op[3]] = ops[op[1]] + ops[op[2]]
      elseif op[0] == 2 then
	 ops[op[3]] = ops[op[1]] * ops[op[2]]
      elseif op[0] == 99 then
	 return ops[0]
      else
	 assert(false)
      end
      op = op + 4
   end
end

local function part1(_ops, size)
   local ops = ffi.new("int[?]", size)
    ffi.copy(ops, _ops, size)
   return run(ops, size, 12, 2)
end

local function part2(_ops, size)
   local ops = ffi.new("int[?]", size)
   for n = 0, 99, 1 do
      for v = 0, 99, 1 do
	 ffi.copy(ops, _ops, size)
	 if(run(ops, size, n, v) == 19690720) then
	    return 100 * n + v
	 end
      end
   end
end

local ops = ffi.new("int[200]")
local opi = 0

while (true) do
   local op = ops + opi
   ffi.C.scanf("%d,%d,%d,%d,", op+0, op+1, op+2, op+3);
   --print(op[0], op[1], op[2], op[3])
   if (op[0] == 99) then
      break
   else
      opi = opi + 4
   end
end

local size = (opi + 1) * 4
print(part1(ops, size))
print(part2(ops, size))
