local ffi = require("ffi")

ffi.cdef[[
int scanf(const char *fmt, ...);
int getchar(void);
]]

local function btree_get_range(tree, min, max)
   local output = {}
   local stack = {}
   while (true) do
      if tree.key == nil then
      elseif tree.key > max then
	 table.insert(stack, tree.left)
      elseif tree.key < min then
	 table.insert(stack, tree.right)
      else
	 table.insert(output, tree.value)
	 table.insert(stack, tree.left)
	 table.insert(stack, tree.right)
      end
      if #stack == 0 then
	 return output
      else
	 tree = table.remove(stack)
      end
   end
end

local function btree_insert(tree, key, value)
   while(true) do
      if tree.key == nil then
	 tree.key = key
	 tree.value = { value }
	 tree.left = {}
	 tree.right = {}
	 return false
      elseif tree.key == key then
	 table.insert(tree.value, value)
	 return true
      elseif tree.key > key then
	 tree = tree.left 
      elseif tree.key < key then
	 tree = tree.right
      else
	 assert(false)
      end
   end
end

local function distance(ax, ay, bx, by)
   return math.abs(ax - bx) + math.abs(ay - by)
end

local function make_line(x,y,x1,y1)
   return
      { ax = math.min(x, x1), ay = math.min(y, y1),
	bx = math.max(x, x1), by = math.max(y, y1) }
end

local function trace(hmemo, vmemo)
   local steps = 0
   local x = 0
   local y = 0
   local x1 = x
   local y1 = y

   while(true) do
      local dir, dist = coroutine.yield()
      if     dir == "U" then y1 = y + dist
      elseif dir == "D" then y1 = y - dist
      elseif dir == "R" then x1 = x + dist
      elseif dir == "L" then x1 = x - dist
      else assert(false)
      end
      
      local line = make_line(x,y,x1,y1)
      local wrap = { line = line, steps = steps, from = {x,y} }
      if x == x1 then
	 table.insert(vmemo, wrap)
      else
	 btree_insert(hmemo, y, wrap)
      end
      
      steps = steps + dist
      x = x1
      y = y1
   end
end

local function score(a, b, ix, iy, best)
   local score = distance(ix, iy, 0, 0)
   if score ~= 0 and score < best[1] then
      best[1] = score
   end

   local alen = distance(a.from[1], a.from[2], ix, iy)
   local blen = distance(b.from[1], b.from[2], ix, iy)
   local score2 = alen + blen + a.steps + b.steps
   if score2 ~= 0 and score2 < best[2] then
      best[2] = score2
   end
end

local function intersect(hmemo, vmemo, best)
   for _, a in ipairs(vmemo) do
      local intersects = btree_get_range(hmemo, a.line.ay, a.line.by)
      for _, x in ipairs(intersects) do
	 for _, b in ipairs(x) do
	    local ix = a.from[1]
	    local iy = a.from[2]
	    iy = b.line.ay
	    if b.line.ax <= ix and ix <= b.line.bx then
	       score(a, b, ix, iy, best)
	    end
	 end
      end
   end
end

local hmemo1 = {}
local vmemo1 = {}
local line1 = coroutine.create(trace)
coroutine.resume(line1, hmemo1, vmemo1)

local hmemo2 = {}
local vmemo2 = {}
local line2 = coroutine.create(trace)
coroutine.resume(line2, hmemo2, vmemo2)

local dirc = ffi.new("char[1]")
local delim = ffi.new("char[1]")
local dist = ffi.new("int[1]")
local line = line1
while (true) do
   ffi.C.scanf("%c%d", dirc, dist);
   
   local dir = ffi.string(dirc, 1)
   coroutine.resume(line, dir, dist[0])

   local delimc = ffi.C.getchar()
   if delimc < 1 then
      break
   else
      local delim = string.char(delimc)
      if delim == "\n" then
	 line = line2
      elseif delim == "," then
      else
	 assert(false)
      end
   end
end

local best = { 9999999, 9999999 }
intersect(hmemo1, vmemo2, best)
intersect(hmemo2, vmemo1, best)
print(best[1])
print(best[2])
