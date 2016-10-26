pico-8 cartridge // http://www.pico-8.com
version 8
__lua__
-- masiakasaurus knopfleri
-- by chase caster
-- special thanks: scott d. sampson & mark knopfler

--------------------------------
-- utilities
--------------------------------

-- class maker
function class(proto, base)
 proto = proto or {}
 proto.__index = proto
 setmetatable(proto, {
  __index = base,
  __call = function(cls, ...)
   local self = setmetatable({
    type=proto
   }, proto)
   if(self.init) self:init(...)
   return self
  end
 })
 proto.subclass = function(subproto)
  return class(subproto, proto)
 end
 return proto
end

-- iterate over table in reverse order
function reverse(t)
 local n=#t+1
 return function()
  n-=1
  if (n>0) return t[n]
 end
end

-- swap keys & values of a table
function invert(t, initial)
 local r={}
 i=initial or 1
 for k in all(t) do
  r[k]=i
  i+=1
 end
 return r
end

-- find key of table item (nil if not found)
function find(t, item)
 for k,v in pairs(t) do
  if (v==item) return k
 end
 return nil
end

-- combine max and min
function bound(v, t, b)
 return max(min(v, t), b)
end

-- randomly choose item from table
function rndchoice(t)
 return t[flr(rnd(#t))+1]
end

-- convert sprite flags to a map layer
function mlayer(...)
 l = 0
 for f in all{...} do
  l+=2^f
 end
 return l
end

-- useful for iterating between x/y & w/h
xywh={x='w', y='h'}

-- is color a darker than color b?
darkindex=invert{
 0,1,2,5,4,3,8,13,9,14,6,11,15,12,10,7
}
function darker(a, b)
 return darkindex[a]<darkindex[b]
end

function isnight()
 return daytime>day-twilight/2 and daytime<day*2-twilight/2
end

-- color mappings for nighttime
nightmap={
 0,1,1,5,0,5,6,4,4,9,3,13,2,8,4
}
function mapnight()
 if (not isnight()) return
 for f, t in pairs(nightmap) do
  pal(f, t)
 end
end

-- increment with hard bounds
function wrap(val, max, min)
 min=min or 0
 local mag=max-min
 if val>=max then
  val-=mag
 elseif val<min then
  val+=mag
 end
 return val
end

-- concatenate multiple list-like tables
function concat(...)
 local r={}
 for l in all{...} do
  for v in all(l) do
   add(r, v)
  end
 end
 return r
end

-- closest integer above x
function ceil(x)
 return -flr(-x)
end

-- round to closest integer
function round(x)
 return flr(x+.5)
end

-- returns 1 if x is positive, -1 if x is negative
function sign(x)
 return x/abs(x)
end

--------------------------------
-- constants
--------------------------------

-- game states
gs_gameover=-1
gs_init=0
gs_play=1
gs_slide=2
gs_sleep=3

gamestate=gs_init

-- sprite flags
sflags=invert({
 'solid',
 'foreground',
 'background',
 'carrion',
 'water',
 'unused',
 'fish',
 'critter',
}, 0)
dt=1/60 --clock tick time
g=9.8/dt -- gravity acceleration
day=60 -- day length in seconds
twilight=6 -- twilight length
maxnum=32767.99

--------------------------------
-- music
--[[ memory format:
0x3100 - song
 each song is 4 bytes
 the first bit of each byte is
 a flag:
  byte 1: loop start
  byte 2: loop back
  byte 3: stop
 the remainder of the byte is
 the index of each of the 4 sfx
 making up the channels of the
 song

0x3200 - sfx
 each sfx is comprised of 64 two
 byte notes, followed by two
 bytes of metadata

note format: 2 bytes, backwards
byte 2:
 0eeevvvi
byte 1:
 iipppppp

 eee: effect (0-7)
 vvv: volume (0-7)
 iii: instrument (0-7, split over bytes)
 pppppp: pitch (0-63)

sfx metadata:
 byte 1 is probably loop start & end
 byte 2 is probably volume
]]
--------------------------------

notenames=invert({
 'c', 'cs', 'd', 'ds', 'e', 'f', 'fs', 'g', 'gs', 'a', 'as', 'b'
}, 0)

function getmusic(m)
 local l=4
 local b=0x3100+m*l
 return {
  i=m, b=b,
  loopstart=band(peek(b),128)!=0,
  loopback=band(peek(b+1),128)!=0,
  stop=band(peek(b+2),128)!=0,
 }
end

musicsoundscache={}
function getmusicsounds(m, skip_cache)
 if (musicsoundscache[m]!=nil) return musicsoundscache[m]

 local music=getmusic(m)
 -- sounds=sounds or {}
 -- if (sounds==nil) sounds={}
 local sounds={}
 for i=0,3 do
  local s=band(peek(music.b+i),127)
  if (s!=0x42) add(sounds, s)
 end
 if not music.loopback and not music.stop then
  sounds=concat(a, getmusicsounds(m+1))
 end
 if (not skip_cache) musicsoundscache[m]=sounds
 return sounds
end

-- todo
-- function setmusic(m, args)
-- end

-- function getsound(s)
--  local l=68
--  local b=0x3200+s*l
--  return {
--   speed=peek(b+65),
--   --todo: loop start / end
--  }
-- end

function setsound(s, args)
 local l=68
 local b=0x3200+s*l
 if args.speed!=nil then
  poke(b+65, args.speed)
 end
 --todo: loop start / end
end

function getnote(s, n)
 local sl=68
 local nl=2
 local b=0x3200+s*sl+n*nl
 local b1=peek(b+1)
 local b2=peek(b)
 return {
  b=b, s=s, n=n,
  pitch=band(b2, 63),
  instrument=shl(band(b1, 1), 2)+shr(band(b2, 192), 6),
  volume=shr(band(b1, 14), 1),
  effect=shr(band(b1, 112), 4),
 }
end

function setnote(s, n, args)
 local sl=68
 local nl=2
 local b=0x3200+s*sl+n*nl
 local b1=peek(b+1)
 local b2=peek(b)
 if args.pitch!=nil then
  poke(b, bor(band(192, b2), args.pitch))
 end
 if args.volume!=nil then
  poke(b+1, bor(band(241, b1), shl(args.volume, 1)))
 end
 -- todo: instrument, effect
end

function reloadmusic(m)
 for s in all(getmusicsounds(m)) do
  reloadsfx(s)
 end
end

function reloadsfx(s)
 local l=68
 local b=0x3200+s*l
 reload(b, l)
end

function altmusic(m, fn)
 for s in all(getmusicsounds(m)) do
  for n=0,63 do
   local nc=fn(getnote(s, n))
   if nc!=nil then
    setnote(s, n, nc)
   end
  end
 end
end

function transpose(m, interval)
 altmusic(m, function(note)
  return {
   pitch=note.pitch+interval
  }
 end)
end

function minorize(m, base)
 base=base%12
 local minors={
  [(base+4)%12]=true,
  [(base+9)%12]=true,
  [(base+11)%12]=true,
 }
 altmusic(m, function(note)
  local pc=note.pitch%12
  if minors[pc] then
   return {
    pitch=note.pitch-1
   }
  end
 end)
end

function settempo(m, speed)
 for s in all(getmusicsounds(m)) do
  setsound(s, {
   speed=flr(speed)
  })
 end
end

function altvolume(m, av)
 altmusic(m, function(note)
  return {
   volume=bound(round(note.volume*av), 7, 0)
  }
 end)
end

--------------------------------
-- coordinate types
--------------------------------
box = class()

function box:init(l,t,w,h)
 self.x=l
 self.l=l
 self.w=w
 self.r=l+w
 self.y=t
 self.t=t
 self.h=h
 self.b=t+h
end

function box:copy()
 return box(self.x,self.y, self.w,self.h)
end

function box:middle()
 return {
  x=self.x+self.w/2,
  y=self.y+self.h/2,
 }
end

-- box contains point
function box:contains(p)
 return (
  p.x>self.l and
  p.x<self.r and
  p.y>self.t and
  p.y<self.b
 )
end

function box:overlaps(other)
 -- don't count touching edges
 -- as overlapping; the right
 -- and bottom are exclusive
 return (
  self.l < other.r and
  self.r > other.l and
  self.t < other.b and
  self.b > other.t
 )
end

function box:parts()
 return self.x, self.y, self.w, self.h
end

--------------------------------
-- particles
--------------------------------

partgen=class()

function partgen:init(args)
 -- required args
 self.x=args.x
 self.y=args.y

 -- optional args
 self.colors=args.colors or {args.color} or {7}
 self.dur=args.duration or maxnum
 self.rate=args.rate or 10
 self.pdur=args.partduration or .2
 self.actor=args.actor

 self.particles={}
 self.age=0
 self.pcount=0
end

-- have i finished drawing
function partgen:done()
 return self.age>self.dur and #self.particles<=0
end

function partgen:stop()
 self.dur=0
end

function partgen:newpart()
 local p={
  x=self.x, y=self.y,
  c=rndchoice(self.colors),
  vel={x=rnd(200)-100, y=-rnd(100)},
  age=0,
 }
 if self.actor then
  p.x+=self.actor.x
  p.y+=self.actor.y
 end
 return p
end

function partgen:update()
 if self.age<=self.dur then
  self.age+=dt
  while self.pcount < self.rate*self.age do
   add(self.particles, self:newpart())
   self.pcount+=1
  end
 end

 for p in all(self.particles) do
  p.age+=dt
  if p.age>=self.pdur then
   del(self.particles, p)
  else
   p.x+=p.vel.x*dt
   p.y+=p.vel.y*dt
   p.vel.y+=g*dt
  end
 end
end

function partgen:draw(o)
 for p in all(self.particles) do
  pset(p.x, p.y, p.c)
 end
end

--------------------------------
-- actor class
--------------------------------
actor = class{
 __name="actor",
 w=1, h=1,
 danger=false,
 critter=false,
 predator=false,
 sprites={},
 sounds={},
}

function actor:init(x,y)
 self.x=x
 self.y=y
 self.vel={x=0,y=0}
 self.acc={x=0,y=0}
 self.slideratio=.2
 self.flipped=false
 self.upsidedown=false
 self.grounded=false
 self.walled=false
 self.wfp=0 --current pixel of walking animation
 self.wfd=8 --number of pixels per frame of walking animation
 self.health=8
 self.pinned=false
end

function actor:spriteset(sprites)
 sprites=sprites or self.sprites
 local set={}
 for _,s in pairs(sprites) do
  if type(s)=="table" then
   for _,ss in pairs(s) do
    set[ss]=true
   end
  else
   set[s]=true
  end
 end
 return set
end

function actor:middle()
 return {
  x=self.x+self.w*4,
  y=self.y+self.h*4,
 }
end

function actor:touch(block)
end

function actor:move()
 --accelerate
 self.vel.x+=self.acc.x*dt
 self.vel.y+=self.acc.y*dt
 self.vel.y+=g*dt

 --upgade walking sprite position
 if self.vel.x==0 then
  self.wfp=0
 elseif self.sprites.walk then
  self.wfp=wrap(
   self.wfp+abs(self.vel.x*dt),
   self.wfd*#self.sprites.walk
  )
 end

 --update coords
 function trymove(axis)
  -- check for collisions along an axis of movement,
  --  pushing the actor away from the collider if
  --  they're only touching at the very edge
  --  (as defined by self.slideratio)

  local dist=self.vel[axis]*dt
  if (dist==0) return -- didn't move
  -- figure out axis & dimension names
  local dim=xywh[axis]
  local opaxis=({x='y', y='x'})[axis]
  local opdim=xywh[opaxis]

  -- build master hit detection box
  --  (1 pixel wide slice just in advance of the hitbox)
  local try=self:hitbox()
  if (dist>0) try[axis]+=self[dim]*8-1
  try[dim]=1

  function slidedir(c)
   -- determine whether collission
   --  coordinate is solid (in the middle)
   --  or needs to slide left or right
   c-=try[opaxis] -- offset of collision from actor origin
   local d=try[opdim] -- width of collision check
   if c<d*self.slideratio then
    return -1
   elseif c>d-(d*self.slideratio) then
    return 1
   end
   return 0
  end

  -- check each pixel of distance travelled for collision
  for i=0,dist,sign(dist) do
   for b in all{try, solidtry, uptry, downtry} do
    -- increment coord of each detection box :(
    b[axis]+=sign(dist)
   end
   local cols=world:collides(try:parts())
   if cols then
    local slide, tile
    for c in all(cols) do
     -- find any slide direction
     slide=slidedir(c[opaxis])
     if slide==0 then
      -- override to no slide if solid
      break
     end
     tile=c.tile
    end
    if slide==0 then -- touching solid ground
     self:touch(tile)
     self.vel[axis]=0
     self[axis]+=i
     return true
    else -- sliding past the edge of ground
     self[opaxis]-=slide
    end
   end
  end
  self[axis]+=dist
  return false
 end
 self.walled = trymove('x')
 self.grounded = trymove('y')
end

function actor:hitbox()
 return box(self.x, self.y,
  self.w*8, self.h*8
 )
end

function actor:overlaps(a)
 return self:hitbox():overlaps(a:hitbox())
end

function actor:sprite()
 if self.sprites.jump then
  if self.vel.y>0 then
   return self.sprites.jump.d
  elseif self.vel.y<0 then
   return self.sprites.jump.u
  end
 end

 if self.vel.x!=0 then
  local wf=flr(self.wfp/self.wfd)+1
  return self.sprites.walk[wf]
 end
 return self.sprites.stand
end

function actor:draw()
 spr(
  self:sprite(),
  self.x, self.y,
  self.w, self.h,
  self.flipped,
  self.upsidedown
 )
end

function actor:eatparticles(rate)
 if self.eating and not self.eatparts then
  local args=self:mouth()
  args.colors={8,8,14}
  args.rate=rate or 50
  self.eatparts=world:particles(args)
 elseif not self.eating and self.eatparts then
  self.eatparts:stop()
  self.eatparts=nil
 end
end

-- a hook to perform actions when an actor gets hurt
-- calls despawn if health runs out
function actor:hurt(d, keep)
 self.health-=d
 if self.health<=0 then
  self.health=0
  if (not keep) world:despawn(self)
 end
end

-- hurt for (at most) d damage, return actual amount hurt
-- (so that munching actor can gain health)
function actor:munch(d)
 local r = min(d, self.health, 0)
 self:hurt(d)
 return r
end


--------------------------------
-- fish class

fish = actor.subclass{
 __name="fish",
 jump=200,
 sprites={
  jump={110, 111},
  flop={126, 127},
  pinned=109,
 },
 sounds={
  splash=35,
 },
 animd=.1,
 critter=true,
}

function fish:init(...)
 actor.init(self, ...)
 self.y+=4
 self.anim=0
 self.vel.y=-self.jump
 self:splash()
 world:particles{
  x=4,y=4, actor=self,
  colors={7,12,1,13},
  rate=10,
 }
end

function fish:hitbox()
 local b = actor.hitbox(self)
 return box(b.x, b.y, b.w, b.h-4)
end

function fish:splash()
 sfx(self.sounds.splash, 2)
 world:particles{
  x=self.x+4, y=self.y+4,
  colors={7,12,1,13},
  duration=0.1,
  rate=20,
 }
end

function fish:touch(s)
 if fget(s, sflags.water) then
  world:despawn(self)
  self:splash()
 elseif fget(s, sflags.solid) then
  self.vel.x=0
 end
end

function fish:move()
 actor.move(self)
 if self.pinned then
  self.upsidedown=false
  self.vel.y=0
  return
 end
 if not self.grounded
   and self:overlaps(protagonist) then
  self.vel.x+=protagonist.vel.x*.75
  self.vel.y+=abs(protagonist.vel.x)*.5
 end
 self.anim+=dt
 if (self.anim>2*self.animd) self.anim=0
 self.upsidedown=self.vel.y>=0
end

function fish:sprite()
 if self.pinned then
  return self.sprites.pinned
 end
 local s=self.sprites.jump
 if self.grounded then
  s=self.sprites.flop
 end
 if self.anim<self.animd then
  return s[1]
 else
  return s[2]
 end
end

--------------------------------
-- critter class

critter = actor.subclass{
 __name="critter",
 run={m=50},
 sprites={
  stand=78,
  walk={78,79},
  pinned=95,
 },
 sounds={
  pinned=32,
 },
 critter=true,
 afraid=true,
}

function critter:init(...)
 actor.init(self, ...)
 self.nextthink=0
 self.flipped=rnd(1)>.5
end

function critter:sprite()
 if (self.pinned) return self.sprites.pinned
 return actor.sprite(self)
end

function critter:think()
 local pb = protagonist:hitbox().b
 local d = protagonist:middle().x - self:middle().x
 if self.afraid and pb>=self.y and pb<=self:hitbox().b and abs(d) < 64 then
  self.vel.x=-sign(d)*self.run.m
  self.nextthink=1
 else
  self.nextthink-=dt
  if self.nextthink<=0 then
   self.vel.x=self.run.m*(flr(rnd(3))-1)
    -- seconds until next movement direction needs choosing
   self.nextthink=rnd(1.5)+.5
  end
 end
end

function critter:move()
 if self.pinned then return end

 self:think()
 actor.move(self)

 if (self.walled) self.vel.x*=-1
 if (self.vel.x!=0) self.flipped=self.vel.x<0
end

--------------------------------
-- rahonavis

rahonavis = critter.subclass{
 __name="rahonavis",
 predator=true,
 run={m=80},
 jump=180,
 sprites={
  stand=76,
  walk={76, 77},
  pinned=93,
  struggle={93, 92},
  eat=75,
  jump=91
 },
 sounds={
  pinned=33,
 },
 danger=true,
}

function rahonavis:init(...)
 critter.init(self, ...)
 self.health=12
 self.eating=false
 self.direction=1
 if self.x>world:pixelbox():middle().x then
  self.direction=-1
 end
 self.rethink=rnd(4)
end

function rahonavis:sprite()
 if (self.eating) return self.sprites.eat
 if (not self.grounded) return self.sprites.jump
 return critter.sprite(self)
end

function rahonavis:mouth()
 local x=6
 if self.flipped then x=2 end
 return {
  x=self.x+x,
  y=flr(self.y)+8,
 }
end

function rahonavis:think()
 self.rethink=self.rethink or rnd(3)
 self.rethink-=dt
 if self.rethink<=0 then
  self.rethink=nil
  self.direction*=-1
 end

 if self.walled and self.grounded then
  self.vel.y-=self.jump
 end
 if self.eating then
  self.vel.x=0
 else
  self.vel.x=self.run.m*self.direction
 end
end

function rahonavis:move()
 critter.move(self)
 self.eating=false

 if not self.pinned then
  for actor in all(world.actors) do
   if not actor.predator and actor:overlaps(self) then
    self.eating=true
    actor.pinned=true
    self.x=actor.x
    actor:munch(dt)
    if actor.health<=0 then
     self.direction=rndchoice{1,-1}
    end
    break
   end
  end
 end
 self:eatparticles(20)

 if self.eatparts then
  local m=self:mouth()
  self.eatparts.x=m.x
  self.eatparts.y=m.y
 end
end

--------------------------------
-- majungasaurus

majungasaurus = critter.subclass{
 __name="majungasaurus",
 critter=false,
 predator=true,
 afraid=false,
 run={m=40},
 sprites={
  head={look=73, eat=89},
  body=70,
  stand=87,
  walk={87,102,104,87,118,120},
 },
 sounds={
  roar=34,
 },
 w=4,
 h=2,
}

function majungasaurus:init(...)
 critter.init(self, ...)
 local tb=world:tilebox()
 self.carrion=0
 self.munched=0
 local i=0
 for x=tb.l,tb.r do
  for y=tb.t,tb.b do
    local s=mget(x, y)
    if fget(s,sflags.carrion) then
     self.carrion+=x*8
     i+=1
    end
  end
 end
 self.carrion/=i
end

function majungasaurus:think()
 local pm=protagonist:middle()
 local sm=self:middle()
 if self.angry then
  self.angry-=dt
  if self.angry<=0 then
   self.angry=nil
  end
 else
  if abs(pm.y-sm.y)<8 and abs(pm.x-sm.x)<32 then
   self.angry=5
   sfx(self.sounds.roar, 2)
  end
 end

 self.eating=false
 if self.angry then
  local dx=pm.x-sm.x
  if dx<8 and pm.y<self.y-4 then
   self.vel.x=0
   self.eating=true
  else
   self.vel.x=self.run.m*sign(dx)
  end
 else
  local hb=self:hitbox()
  if hb:contains{x=self.carrion,y=self.y+1} then
   self.vel.x=0
   self.eating=true
  else
   self.vel.x=self.run.m*sign(self.carrion-hb.x)
  end
 end
end

function majungasaurus:mouth()
 local m={
  x=self.x+4,
  y=self.y+10
 }
 if (not self.flipped) m.x+=24
 return m
end

function majungasaurus:move()
 critter.move(self)
 self:eatparticles(10)
 if self:overlaps(protagonist) then
  local mm=self:middle()
  local pm=protagonist:middle()
  local dx=mm.x-pm.x

  if (self.munched>0) self.munched-=dt
  if self.munched<=0 and protagonist.y>self.y-6 then
   if self.flipped==(dx>0) then
    self.munched=0.2
    protagonist.vel.x=-sign(dx)*200
    protagonist.vel.y-=80
    protagonist:munch(4)
    world:particles{
     x=(mm.x+pm.x)/2,
     y=(mm.y+pm.y)/2,
     color=8,
     duration=0.2,
     rate=500,
    }
   else
    protagonist.vel.x=-sign(dx)*50
   end
  else
    protagonist.vel.y=-80
    protagonist.vel.x=-dx*10
  end
 end
end

function majungasaurus:legsprite()
 if self.vel.x!=0 then
  local wf=flr(self.wfp/self.wfd)+1
  return self.sprites.walk[wf]
 end
 return self.sprites.stand
end

function majungasaurus:draw()
 palt(0, false)
 palt(12, true)

 local hx=self.x
 local hy=self.y
 local bx=hx
 if self.flipped then
  bx+=8
 else
  hx+=24
 end
 spr(self.sprites.body,
  bx, self.y, 3,1,
  self.flipped
 )
 local hsk="look"
 if (self.eating) hsk="eat" hy+=3
 spr(self.sprites.head[hsk],
  hx, hy, 1,1,
  self.flipped
 )
 spr(self:legsprite(),
  self.x+8, self.y+8, 2,1,
  self.flipped
 )

 palt()
end

--------------------------------
-- player class

player = actor.subclass{
 __name="player",
 run={a=600, m=80},
 jump=100,
 btn={j=5,l=0,r=1,c=3,e=4,s=4},
 jd=dt*4, --0-1 in 1/4 seconds
 w=2,
 sprites={
  stand=64,
  walk={80,64,96,64},
  crouch=66,
  sleep=112,
  dead=114,
  jump={u=82,d=98},
  eat={68, 84, fc={16,8}},
  drink=100,
 },
}

function player:init(...)
 actor.init(self, ...)
 self.slideratio=0.25
 self.j=0
 self.eating=false
 self.drinking=false
 self.crouched=true
 self.hurtflash=0
 self.es=1
 self.esd=0
 self.efc=8
 self.food={}
 self.stats={
  health=1,
  food=.8,
  water=.9,
  sleep=0.5,
 }
 self.btndelay={}
 self.score={
  food=0,
  water=0,
  sleep=0,
  critter=0,
  rahonavis=0,
  fish=0,
  days=0,
 }
end

function player:munch(d)
 self.health=self.stats.health*10
 actor.munch(self, d)
 self.stats.health=self.health/10
end

function player:sprite()
 if gamestate==gs_gameover then
  return self.sprites.dead
 elseif self.sleeping then
  return self.sprites.sleep
 elseif self.crouched then
  if self.sleepcount and self.sleepcount>0.1 then
   return self.sprites.sleep
  else
   return self.sprites.crouch
  end
 elseif self.drinking then
  return self.sprites.drink
 elseif self.eating then
  self.esd+=1
  if self.esd>=self.sprites.eat.fc[self.es] then
   self.esd=0
   self.es+=1
   if self.sprites.eat[self.es] == nil then
    self.es=1
   end
  end
  return self.sprites.eat[self.es]
 elseif self.j>0 then
  return self.sprites.crouch
 end
 return actor.sprite(self)
end

-- is player standing on water
function player:onwater()
 local m=self:mouth()
 return world:collides(m.x, m.y+1, 1, 1, sflags.water)
end

-- coords of my mouth
function player:mouth()
 local c=self:middle()
 c.y=self:hitbox().b
 c.x+=4
 if (self.flipped) c.x-=8
 return c
end

-- delayed button detection
function player:dbtn(b)
 local d=bound(.33-self.stats.sleep, 0.33, 0)*1.5
 local r=false
 if (self.btndelay[b]==nil) self.btndelay[b]=0
 if btn(b) then
  r=self.btndelay[b]>=d
  self.btndelay[b]+=dt
 else
  self.btndelay[b]=0
 end
 return r
end

function player:move()
 self.hurting=false --until proved otherwise
 if self.sleeptime then
  if (band(btnp(), 0xf7) != 0) self.sleeptime+=5
  return
 end
 local slpadj=bound(self.stats.sleep*2, 1, 0.3)

 local run=self.run.m*slpadj
 local jump=self.jump*slpadj
 if btn(self.btn.s) then
  run*=1.5
  jump*=1.1
 end

 if btn(self.btn.l) then
  self.flipped=true
 elseif btn(self.btn.r) then
  self.flipped=false
 end
 if self:dbtn(self.btn.j) and self.grounded then
  self.j=min(self.j+self.jd,1)
 elseif self.j>0 then
  self.vel.y-=jump*(1+self.j)
  self.j=0
 end

 self.crouched=btn(self.btn.c)
 if self.crouched then
  if self.sleepcount==nil then
   self.sleepcount=0
  elseif not self:onwater() and self.grounded then
   self.sleepcount+=dt
   if self.sleepcount>0.6 then
    self.sleepcount=nil
    self.sleeping=true
   end
  end
  self.vel.x=0
 elseif self:dbtn(self.btn.l) and self.j<=0 then
  self.vel.x-=self.run.a*dt
  self.vel.x=max(self.vel.x,-run)
 elseif self:dbtn(self.btn.r) and self.j<=0 then
  self.vel.x+=self.run.a*dt
  self.vel.x=min(self.vel.x,run)
 elseif self.vel.x!=0 then
  local s=sign(self.vel.x)
  self.vel.x+=-s*self.run.a*dt
  if sign(self.vel.x)!=s then
   self.vel.x=0
  end
 end

 if (not self.crouched) self.sleepcount=0

 -- drink/eat
 self.eating=false
 self.drinking=false
 local hb=self:hitbox()
 if self.grounded and self:dbtn(self.btn.e) then
  if #self.food>0 then
   self.eating=true
   self:eat()
  elseif world:carrionvisible() and world:collides(hb.x,hb.y,hb.w,hb.h, sflags.carrion) then
   self.eating=true
   self.stats.food+=dt*.05
   self.stats.water+=dt*.025
  elseif self:onwater() and abs(self.vel.x)<dt then
   self.drinking=true
   self:drink()
  end
 end
 if (self.eating or self.drinking) self.vel.x=0 self.acc.x=0
 if (not self.eating) self.es=1 self.esd=0

 self:eatparticles()
 if self.drinking and not self.drinkparts then
  local args=self:mouth()
  args.colors={7,12,1,13}
  args.rate=8
  self.drinkparts=world:particles(args)
 elseif not self.drinking and self.drinkparts then
  self.drinkparts:stop()
  self.drinkparts=nil
 end

 actor.move(self)

 -- make sure we're still pinning food
 for f in all(self.food) do
  if not self:overlaps(f) then
   f.pinned=false
   del(self.food, f)
  elseif self.vel.y>0 or self.vel.x+self.vel.y==0 then
   f.y=self.y f.x=self:mouth().x-4
  end
 end
end

-- reset sleep counter if player gets hurt
function player:hurt(d)
 actor.hurt(self, d, true)
 self.hurting=self.health>0
 self.sleepcount=0
end

function player:draw()
 if self.hurtflash>0.2 then
  pal(13, 8)
  pal(5, 8)
  pal(15, 14)
  pal(6, 14)
 end
 actor.draw(self)
 pal()
end

-- decrement stats
function player:age(dt)
 if self.hurting then
  self.hurtflash-=dt
  if self.hurtflash<=0 then
   self.hurtflash=0.4
   sfx(0)
  end
 else
  self.hurtflash=0
 end

 local d=(1/day)*dt
 local s=abs(self.vel.x)/self.run.m
 self.stats.water-=d/3
 self.stats.food-=(d/8)+((s^3)/6000)
 self.stats.sleep-=d/3
 if isnight() then
  self.stats.sleep-=d/5
 else
  self.stats.water-=d/3
 end

 local hd=(1-min(self.stats.food*2, 1))*2
 hd+=(1-min(self.stats.water*2, 1))*2
 self.stats.health-=d*hd
 for k,v in pairs(self.stats) do
  self.stats[k]=min(max(v,0),1)
 end

 if self.stats.sleep<=0 then
  self.sleepcount=nil
  self.sleeping=true
 end
end

function player:addstat(key, val)
 self.stats[key]+=val
 self.score[key]+=val
end

function player:drink()
 self:addstat('water', dt/6)
end

function player:eat()
 local f=self.food[1]
 local a=f:munch(8*dt)/100
 self:addstat('food', a)
 self:addstat('water', a/2)
 self.stats.health+=a/8
 f.x=self:mouth().x-f.w*4
 f.y=self.y
 f.flipped=self.flipped
 if f.health<=0 then
  del(self.food, f)
  if self.score[f.__name]!=nil then
   self.score[f.__name]+=1
  end
 end
end

function player:findfood(actors)
 if (self.vel.y<=0) return
 for a in all(actors) do
  if a.critter and a.y>=self.y and self:overlaps(a) then
   if not a.pinned then
    sfx(a.sounds.pinned, 2)
   end
   a.pinned=true
   add(self.food, a)
  end
 end
end

function player:snooze(dt)
 if self.sleeptime==nil then
  self.sleeptime=0
 else
  self.sleeptime+=dt
 end
 self.score.sleep+=dt

 local d=(1/day)*dt
 self.stats.water-=d/6
 self.stats.food-=d/5
 self.stats.sleep+=d/3
 if (isnight()) self.stats.sleep+=d/5

 local hd=1/3
 hd+=min(self.stats.food*2-1, 0)
 hd+=min(self.stats.food*2-1, 0)/3
 self.stats.health+=d*hd
 for k,v in pairs(self.stats) do
  self.stats[k]=min(max(v,0),1)
 end

 -- check whether we woke up
 if (self.stats.sleep>=1
   or self.sleeptime>50) then
  self.sleeptime=nil
  self.sleeping=false
  return true
 end
 return false
end

--------------------------------
-- the world
--------------------------------
world={
 o={
  x=0,
  y=16,
 },
 screens={
  w=8,h=4,
  x=0,y=0,
  d={x=0,y=0},
 },
 tiles={
  w=16,h=14,
  d={x=0,y=0},
 },
 pixels={
  w=8,h=8,
 },
 stars={},
 partgens={},
 actors={},
 critterpop={},
 maxpop=3,
 spawns={},
 nextfish=rnd(10),
 prespawn={},
 carrion={},
 hadmajung=false,
}

function world:makestars(n)
 colors={1,5,6,7,13}
 for i=1,n do
  add(self.stars, {
   x=flr(rnd(128)),
   y=flr(rnd(128)),
   c=rndchoice(colors)
  })
 end
end

function world:tilebox()
 return box(
  self.screens.x*self.tiles.w+self.tiles.d.x,
  self.screens.y*self.tiles.h+self.tiles.d.y,
  self.tiles.w,
  self.tiles.h
 )
end

function world:pixelbox()
 local b=self:tilebox()
 return box(
  b.x*self.pixels.w,
  b.y*self.pixels.h,
  b.w*self.pixels.w,
  b.h*self.pixels.h
 )
end

-- pixel offset of the screen
function world:offset()
 local pb=self:pixelbox()
 return {
  x=pb.x-self.o.x,
  y=pb.y-self.o.y,
 }
end

-- unique key for current screen
function world:screenkey()
 return self.screens.x..","..self.screens.y
end

-- add a partcle generator to the world
function world:particles(args)
 local p=partgen(args)
 add(self.partgens, p)
 return p
end

-- add an actor to the world
function world:spawn(actor)
 if actor!=nil and find(self.actors, actor)==nil then
  add(self.actors, actor)
 end
end

-- remove an actor from the world
function world:despawn(actor)
 if find(self.actors, actor)!=nil then
  if actor.critter then
   local s=self:screenkey()
   self.critterpop[s]=max(self.critterpop[s]-1, 0)
  end
  del(self.actors, actor)
 end
 for pg in all(self.partgens) do
  if (pg.actor==actor) pg:stop()
 end
end

-- spawn the player
function world:spawn_protagonist()
 local b=self:tilebox()
 local p={x=32,y=64}
 for x=b.x, b.w+b.x do
  for y=b.y, b.h+b.y do
   if mget(x,y)==64 then
    p={x=x*self.pixels.w,y=y*self.pixels.h}
    break
   end
  end
 end
 local p=player(p.x, p.y)
 self:spawn(p)
 return p
end

function world:carrionvisible()
 return self.carrion[self:screenkey()]
end

-- spawn all visible critters
function world:findspawns()
 -- should this screen have carrion
 local s=self:screenkey()
 if self.carrion[s]==nil then
  self.carrion[s]=rnd()>0.5
 end

 for a in all(self.actors) do
  if a.critter then
   local b=world:checkbounds(a:middle())
   if (b.x+b.y!=0) del(world.actors, a)
  end
 end
 for p in all(self.partgens) do
  p:stop()
  del(self.partgens, p)
 end

 local b=self:tilebox()
 self.spawns.critters={}
 self.spawns.danger={}
 self.spawns.apex={}
 self.spawns.fish={}

 -- find the critters on the map
 for x=b.x, b.w+b.x do
  for y=b.y, b.h+b.y do
   local spawninfo={x=x*self.pixels.w, y=y*self.pixels.h}
   local s=mget(x,y)
   if fget(s,sflags.critter) then
    if majungasaurus:spriteset()[s] then
     if self:carrionvisible() then
      spawninfo.type=majungasaurus
      add(self.spawns.apex, spawninfo)
     end
    elseif rahonavis:spriteset()[s] then
     spawninfo.type=rahonavis
     add(self.spawns.danger, spawninfo)
    else
     add(self.spawns.critters,
      critter(spawninfo.x, spawninfo.y))
    end
   elseif fget(s,sflags.fish) then
    add(self.spawns.fish, spawninfo)
   end
  end
 end

 self:spawn_critters()
end

function world:spawn_critters()
 -- choose from available based on population
 local s=self:screenkey()
 local cn=3--#self.spawns.critters
 if self.critterpop[s]==nil or self.critterpop[s]>cn then
  self.critterpop[s]=3
 end
 for i=1,self.critterpop[s] do
  local ids={}
  for i=1,#self.spawns.critters do add(ids, i) end
  local ci=rndchoice(ids)
  del(ids, ci)
  local c=self.spawns.critters[ci]
  self:spawn(c)
 end
end

function world:spawn_danger()
 if self.critterpop[self:screenkey()]>0 then
  local c=rndchoice(self.spawns.danger)
  if c then
   self:spawn(c.type(c.x, c.y))
  end
 end
 if #self.spawns.apex>0 then
  c=rndchoice(self.spawns.apex)
  if not self:hasapex(c.type) then
   self:spawn(c.type(c.x, c.y))
  end
 end
end

function world:spawn_fish()
 if #self.spawns.fish>0 then
  local s=rndchoice(self.spawns.fish)
  s.pre=.5
  add(self.prespawn, s)
  self:particles{
   x=s.x+4, y=s.y+8, colors={7},
   duration=s.pre,
   rate=5,
  }
 end
end

function world:hasapex(type)
 type=type or majungasaurus
 for a in all(self.actors) do
  if (a.type==type) return true
 end
 return false
end

-- advance daytime
function world:advance(dt)
 daytime+=dt
 if daytime>day*2 then
  daytime=0
  world:morning()
 end
 for p in all(self.partgens) do
  p:update(dt)
  if p:done() then
   del(self.partgens, p)
  end
  for s in all(self.prespawn) do
   s.pre=s.pre or 0
   s.pre-=dt
   if s.pre<=0 then
    s.pre=nil
    self:spawn(fish(s.x, s.y))
    del(self.prespawn, s)
   end
  end
 end

 local pop=self.critterpop[self:screenkey()]
 local popadj=4-pop
 self.nextdanger=self.nextdanger or (3+rnd(5))*popadj
 self.nextdanger-=dt
 if self.nextdanger<=0 then
  self.nextdanger=nil
  self:spawn_danger()
 end
 if gamestate!=gs_sleep then
  self.nextfish-=dt
  if self.nextfish<=0 then
   self.nextfish=rnd(2)*(1+popadj)
   self:spawn_fish()
  end
 end

 if self.hadmajung!=self:hasapex() then
  self.hadmajung=not self.hadmajung
  updatemusic()
 end

 world:move_actors()
end

function world:move_actors()
 for a in all(self.actors) do
  a:move()
  local b=self:checkbounds(a:middle())
  if b.x!=0 or b.y!=0 then
   if a==protagonist then
    gamestate=gs_slide
    self:translate(b)
   else
    if fading[a]==nil then
     fading[a]=3
     if (gamestate==gs_sleep) fading[a]=0
     local pa=self:wrappoint{x=a.x,y=a.y}
     a.x=pa.x
     a.y=pa.y
    else
     fading[a]-=dt
     if fading[a]<=0 then
      del(world.actors, a)
      fading[a]=nil
     end
    end
   end
  end
 end
end

-- a new day has dawned, update stuff
function world:morning()
 for s,p in pairs(self.critterpop) do
  self.critterpop[s]=min(p+1, 3)
 end
 -- reset whether any tile has carrion
 self.carrion={}
 protagonist.score.days+=1
end

-- player has gone to sleep
function world:startsleep()
 for a in all(self.actors) do
  if a!=protagonist then
   del(self.actors, a)
  end
 end
 for pg in all(self.partgens) do
  pg:stop()
 end
end

-- check for collisions in box
function world:collides(x,y,w,h, flag)
 flag = flag or sflags.solid
 local res = {}
 for nx=x,x+w-1 do
  for ny=y,min(y+h-1, 448) do --448 is the edge of the map
   local s=mget(flr(nx/8),flr(ny/8))
   if fget(s,flag) then
    add(res, {x=nx, y=ny, tile=s})
   end
  end
 end
 if (#res<=0) return false
 return res
end

-- check whether point is outside bounds
function world:checkbounds(p)
 local b=self:pixelbox()
 local res={x=0,y=0}

 if p.x<b.l then
  res.x=-1
 elseif p.x>b.r then
  res.x=1
 end

 if p.y<b.t then
  res.y=-1
 elseif p.y>b.b then
  res.y=1
 end

 return res
end

-- wrap a coordinate point around the map
function world:wrappoint(p)
 local w=self.screens.w*self.tiles.w*self.pixels.w
 local h=self.screens.h*self.tiles.h*self.pixels.h
 p.x=wrap(p.x, w)
 p.y=wrap(p.y, h)
 return p
end

-- move viewport to next screen
function world:translate(d)
 if d then
  self.screens.d=d
  self.tiles.d={x=0,y=0}
 end
 local sd=self.screens.d
 local td=self.tiles.d
 local done=false
 for k,s in pairs(xywh) do
  if sd[k]!=0 then
   td[k]+=sd[k]
   if abs(td[k])>=abs(self.tiles[s]) then
    done=true
   end
  end
 end
 if done then
  for k,s in pairs(xywh) do
   self.screens[k]+=self.screens.d[k]
   local w=wrap(self.screens[k], self.screens[s])
   if w!=self.screens[k] then
    self.screens[k]=w
    protagonist[k]+=self:pixelbox()[s]*self.screens[s]*-sign(self.screens.d[k])
   end
  end
  self.screens.d={x=0,y=0}
  self.tiles.d={x=0,y=0}
 end
 return done
end

function world:drawsky()
 local starcolors={12,13,2,1,0}
 local cn=#starcolors-2
 local ci=1
 if daytime>day then
  if daytime>=day*2-twilight then
   ci+=flr((day*2-daytime)*cn/twilight)
  else
   ci=#starcolors
  end
 elseif daytime>=day-twilight then
  ci+=cn-flr((day-daytime)*cn/twilight)
 end
 rectfill(0,0,127,127,starcolors[ci])
 if ci>1 then
  for s in all(self.stars) do
   if darker(starcolors[ci], s.c) and rnd()>0.02 then
    pset(s.x, s.y, s.c)
   end
  end
 end
end

function world:draw()
 self:drawsky()
 local tb=self:tilebox()
 local pb=self:pixelbox()

 -- compute wrapping coordinates
 local w={
  c={
   x=tb.x,y=tb.y,
   w=tb.w,
   h=tb.h,
  },
  p={x=0,y=0},
 }
 local wrapping=false
 for k,s in pairs(xywh) do
  local ws=self.screens[k]+self.screens.d[k]
  if ws<0 or ws>=self.screens[s] then
   wrapping=true
   w.c[k]=0
   w.c[s]=abs(self.tiles.d[k])
   w.p[k]=0
   if self.tiles.d[k]!=0 then
    if self.tiles.d[k]<0 then
     w.c[k]=tb[k]+self.tiles[s]*self.screens[s]
    else
     w.p[k]=(self.tiles[s]-self.tiles.d[k])*self.pixels[s]
    end
   end
  end
 end

 function drawmaps(layer)
  camera(pb.x-self.o.x, pb.y-self.o.y)
  map(
   tb.x,tb.y,
   pb.x,pb.y,
   tb.w, min(tb.h, 56-tb.y),
   layer
  )
  if wrapping then
   map(
    w.c.x, w.c.y,
    pb.x+w.p.x, pb.y+w.p.y,
    w.c.w, min(w.c.h, 56-w.c.y),
    layer
   )
  end
  pal()
 end

 -- do the actual drawing
 mapnight()
 local bgl=mlayer(sflags.background)
 if self.carrion[self:screenkey()] then
  bgl=mlayer(sflags.background, sflags.carrion)
 end

 local offset=self:offset()

 -- draw background
 drawmaps(bgl)

 -- draw actors
 for a in reverse(self.actors) do
  a:draw()
 end

 camera()
 mapnight()
 -- draw water
 for wx=tb.l,tb.r-1 do
  for wy=tb.t,tb.b-1 do
    local s=mget(wx, wy)
    if fget(s,sflags.water) then
     wx=wx*8-offset.x
     wy=wy*8-offset.y
     spr(22, wx,wy)
     spr(6, wx,wy-8)
    end
  end
 end

 -- draw foreground
 mapnight()
 drawmaps(mlayer(sflags.foreground, sflags.solid))

 -- draw particles
 for p in all(self.partgens) do
  p:draw(offset)
 end
 camera()
end

--------------------------------
-- the hud
--------------------------------

hudflash={
 health=0, water=0, food=0, sleep=0
}

function flash(k, m, t)
 local result=false
 t=t or 0.5
 local l=0.2
 if m<=t then
  if hudflash[k]<=0 then
   hudflash[k]=l*6*m/t+l*2
  else
   hudflash[k]-=dt
  end
  if hudflash[k]<=l then
   result=true
  end
 end
 return result
end

function drawahud(s, bc, x,y, k)
 local m=protagonist.stats[k]
 if flash(k, m) then
  bc=8
 end
 rectfill(x+2,y, x+61*m,y+7, bc)
 spr(s, x,y)
 for c=1,6 do
  spr(s+1, x+c*8,y)
 end
 spr(s+2, x+56,y)
end

function drawhud()
 mapnight()
 rectfill(0,0,127,15,0)
 drawahud(13,8, 0,0, 'health')
 drawahud(29,13, 0,8, 'food')
 drawahud(45,12, 64,0, 'water')
 drawahud(61,7, 64,8, 'sleep')
 pal()
end

--------------------------------
-- the game
--------------------------------

function updatemusic()
 local m=0
 reloadmusic(m)
 if gamestate==gs_gameover then
  settempo(m, 20)
  transpose(m, -3)
 elseif world:hasapex() then
  minorize(m, notenames.e)
 else
  if isnight() then
   transpose(m, -1)
  end
  if gamestate==gs_sleep then
   settempo(m, 20)
   altvolume(m, .8)
  end
 end
end

function _init()
 protagonist=world:spawn_protagonist()
 world:findspawns()
 world:makestars(128)
 daytime=0
 wakingtime=0
 sleeptime=0

 fading={}
end

splashoff=55
wasnight=false
function _update60()
 if gamestate==gs_gameover then
  gotime+=dt
  if gotime>8 and (btnp(4) or btnp(5)) then
   run()
  end
  return
 elseif gamestate==gs_init then
  splashoff=wrap(splashoff+dt*player.run.m, 127, -16)
  if btn(4) or btn(5) then
   initqueued=true
  elseif initqueued then
   gamestate=gs_play
   music(0)
  else
   return
  end
 elseif gamestate==gs_play then
  world:advance(dt)
 elseif gamestate==gs_slide then
  if world:translate() then
   gamestate=gs_play
   world:findspawns()
  else
   return
  end
 elseif gamestate==gs_sleep then
  local sdt=dt*10
  sleeptime+=dt
  world:advance(sdt)
  if protagonist:snooze(sdt) then
   gamestate=gs_play
   updatemusic()
   sleeptime=0
  end
  return
 end
 if isnight()!=wasnight then
  wasnight=isnight()
  updatemusic()
 end

 protagonist:findfood(world.actors)
 protagonist:age(dt)

 if protagonist.stats.health<=0 then
  gamestate=gs_gameover
  gotime=0
 elseif protagonist.sleeping then
  gamestate=gs_sleep
  world:startsleep()
  updatemusic()
 end
end

function drawsplash()
 rectfill(0,0,127,127,12)
 palt(0,false)
 spr(224, 0,3, 16,2)
 palt()

 print("mark knopfler's vicious lizard", 5, 24, 2)
 print("hold [x] to jump\nhold [o] to run or eat\nhold [down] to sleep", 28,44, 7)
 c=1
 if (initqueued) c=14
 print("press any button to start", 24,68, c)

 for x=0,120,8 do
  spr(1, x, 120)
  if ((x+16)%32==0) spr(28, x, 112)
 end
 local sprites=player.sprites.walk
 spr(sprites[(flr(splashoff/8)%#sprites)+1], splashoff, 112, 2,1)
end

function drawgameover()
 local p=protagonist:middle()
 local o=world:offset()
 local drawspeed=2
 p.x-=o.x
 p.y-=o.y-2
 local d=(1-min(gotime/drawspeed, 1))*127

 -- black out screen
 color(0)
 rectfill(0,0,127,p.y-d)
 rectfill(0,p.y+d,127,127)

 camera(o.x,o.y)
 protagonist:draw()
 camera()

 if d<=0 then
  -- draw red line
  d=(min(gotime/drawspeed-1, 1))*max(p.x, 127-p.x)
  clip(p.x-d, 0, d*2, 127)
  line(0, p.y+1, 127, p.y+1, 8)
  local tx=p.x+16
  if p.x>64 then
   tx-=64
  end
  print("game over", tx, p.y-4, 8)
  clip()
 end

 -- draw stats
 if gotime>drawspeed*2 then
  if (protagonist.y-o.y<111) protagonist.y+=1
  local score=protagonist.score
  local txt="days survived: "..score.days..
   "\nslept: "..flr(score.sleep)..
   "\ndrank: "..flr(score.food*100)..
   "\nate: "..flr(score.food*100)..
   "\n - "..score.critter.." mammals"..
   "\n - "..score.fish.." fish"..
   "\n - "..score.rahonavis.." rahonavii \n\n press a button to restart"
  local f=(gotime-drawspeed*2)*10
  if (f<#txt) txt=sub(txt, 0, f-#txt)
  print(txt, 8,8, 4)
 end
end

function drawsleep()
 local p=protagonist:middle()
 local o=world:offset()
 for i=0,2 do
  local x=p.x-6+i*4
  local y=p.y-10+sin((sleeptime+dt*4*i))*3
  print("z", x-o.x, y-o.y, 9)
 end
end

waterdt=0
wateroff=1
waterdist=0
function movewater()
 waterdt+=.25
 if waterdt>60 then
  waterdt=0
  wateroff*=-1
 end
 waterdist+=(-sin(waterdt/120))*.15
 if (waterdist<1) return

 for tile in all{6,22} do
  local x_origin=(tile%16)*8
  local y_origin=flr(tile/16)*8
  for y=y_origin,y_origin+7 do
   local row={}
   for x=x_origin,x_origin+7 do
    add(row, sget(x,y))
   end
   for x=0,7 do
    local c=row[x+1]
    x+=wateroff*flr(waterdist)
    if (x>7) x-=8
    if (x<0) x+=8
    x+=x_origin
    sset(x,y, c)
   end
  end
 end
 waterdist-=flr(waterdist)
end

function _draw()
 if (gamestate==gs_gameover) drawgameover()
 if (gotime and gotime>0) return
 if (gamestate==gs_init) drawsplash(); return
 movewater()
 world:draw{x=0, y=16}
 drawhud()
 if (gamestate==gs_sleep) drawsleep()
end

__gfx__
70000007b3bb3bbb000000006363366366666665000000000000000011121111bb3b1bb3b3bb3bbb116555510bb0bbb00333033055ee2ee55555555555555555
070000703b33b3b3000000003633623566666255000000000000000011111151bbb2b3213b33b3b311161511b00b0000000030035e70e07e5ec55ec55ec55555
00700700a4493b34000000006565556565655565000000000000000011edd211b1b333b3a4493b34151655120004f0bb330940005700000ee7eee7eee7eeee55
000770009a94934233000033665625126656251260060000000000001edddd21bb3b3b139a94934211115111000f9400009440005e00000000e0000000e000e5
00077000949444449a434422625555516255555156652552000000001d555521b2b311b19494444411156111b00f40000009f0035700000000000e00000000e5
00700700499442424994424256551511565515116255555100000000111dd2113131231149944242111651110b0990000004203052e000eeee7eee7eee7eee55
070000709444442294444422255251512552515125525151000d00001211dd212b1b1131944444221211515100f94000000f4200557e0e7555ec55ec55ec5555
70000007442422224424222251511111515111115151111100d6100111111d1113111111442422221111611100049000000420005552e2555555555555555555
bb3b1bb39aaa994903bb3bbbb3bb3bb003bb3bb011111111ddcc71169aaa994966666665444444441111611100b00b000030030054f555555555555555555555
bbb2b3219a9949443b33b3b33b33b3b33b33b3b311111151c6c76c7c9a9949446666625594444f9411115151b00bb000000330035f0ffff54ff54ff54ff55555
b1b333b3a44994443bb33b34a4493b333bb33b33151111116551ccc1a449944465655565f9449fff15165112b000b00b300300035f00f00ff00ff00ff00fff45
bb3b3b139a949442bb3993429a94933bbb34933b111112117ccc1d6c9a94944266562512ff49fff9111561110b0bb0b0030330305f000000f000f000f000f0f5
b2b311b194944444b394444494944433b394443311111111c1c61ccc9494444462555551ff4ffff41111511100bffb000039f3005f0000f000f000f000f000f5
31312311499442423b344242499442b33b3442b311151111dc7c6cd64994424256551511ff9f9fff11165511b0f4990b3094420354f00ff00ff00ff00ff00f45
2b1b113194444422b344442294444432b3444432121d1111c16d1d6c94444422255251519fff4f9f121615110b4494b003449230554ff54ff54ff54ff54ff555
131111114424222234242222442422223424222211d6d11d16c16c1644242222515111114fffff4f116555510039430000142100555555555555555555555555
4a429492444444440000000000000000000000000000000000595200000000055959452511111111111111110000000000000000551111555555555555555555
a944424294444f940000000000000000000000000000000000952900000000499442524011251111111511110330033001100110510110155111555551115555
94a49294f9449fff000000000000000000000000000000000029240000000945595442001111211111f44211b030320000210103510000111000155110001555
44942424ff49fff99040440400000000000000000000000000545200000095225242400012f11f211f4444210bb0300000010330510000100000011000000155
a4444242ff4ffff4f449f449000000000000000000000000009592000004544494540000115442111455552133b300bb33001311510000000110000001100015
a9492492ff9f9fffff9f9fff0000000000000000000000000099420000949424424000002f1541421514411100bb3bb223313300510000011551000115510015
a49494229fff4f9f9fff4f9f00000000000000000000000000425500094525544200000015f42451111f4511002bbb3001333200551001155555111555551155
94a429424fffff4f4fffff4f0000000000000000000000000059450059454452500000005f54452511f44211000bb00000033000555115555555555555555555
0000000440000000444444444444444400000004400000005959452b595945255000000012211d2102200d201111111159594525565665555555555555555555
000000449400000004444f9494444f90000000449400000099445b33094452424900000011d1122100d002201111115199445245556006566556655666555555
0000044ff440000000f49ffff94449000000044ff44000005959333500594445545000001d2111210d2000201511111159594245650000600660066000666666
00004449f94400000009fff9ff49f00000000449f9400000544b35240009452454490000122111d2022000d21111121154494524560000000000000000000065
0004f494f94440000000fff4ff4f000000000494f940000094333424000054449454500011211122002000221111111194545424560000000000000000000655
004499ffff4f940000000fffff900000000009ffff4000009b3424240000094549444900111511d1000000d01115111199442424650000600660066000666555
049f49ff9f9ff4400000009f9f000000000000ff9f000000b3352554000000542445255012111111000000001211111149452554556006566556655666555555
4ff9ff4f4ffff9440000000f400000000000000f4000000033454452000000095425425411111511000000001111111159454452565665555555555555555555
ddd00000000dd0000000000000000000ddd0000000000000cccccccccccc22222cccccccccccc9cc000000000000000000000000000000000000000000000000
0ffddd0000ddbd5000000000000000000ffddd0000000000cccccccc22220220022222ccc424499c000000000000000000000000000000000000000000000000
000fffdddddff556ddd00000000dd000000fffdddddddd002222222222000020002200222428244c000000000000000000000180000018000000000000000000
00000ffffdd000600ffddd0000ddbd5000000ffffddfddd0c000200002000020000000024244442400000000100000001000011a100011a00000000000000000
00000666ddd00000000fffdd0ddff55600000666fddfbdd0ccc0000000000000000000222442424400000000d1111100d1111100d1111100f0000440f0000440
0000666f0000000000000ffffdd000600000666fdd056500cccccc000000000000020222424999cc000000000dd111100dd111100dd111100f44455e0f44455e
000066550000000000000666ddd000000000665500665000ccccccccc0000000002002222242499c000000000061118000566010006665100049490000494900
000006655000000000000665500000000000066550ee6000ccccccccccc0000000200ccccc24444c0000000000561a0000f56a00006a5f000065065000560560
0000000000000000ddd00000000dd0000000000000000000cccccccccccc000455cccccc2422cccc000000000000000000000000000000000000000000000000
dddd0000000dd0000ffddd0000ddbd50ddd0000000dbd560cccccccccccc00445ccccccc422424cc00000000011a000000000000000000000000000000000000
0fffddddddddbd50000fffdddddff5560ffddd000dddd5eecccccc22cccc4445cccccccc242284cc000000000011100000000000000000000000000000000000
0000ffffddfff55600000ffffdd00060000fffddddfff66eccc22200ccc42255cccccccc44442499000000001001118000000000000000000000000000000000
0000666ddd00006000000666fddd000000000ffffd000000c2202000ccc444555ccccccc2249442c00000000d111111a1000000a000000000000000065000560
0006666f55500000000066600dd0000000000666ddd0000020000000cccc424555ccccccc4299244000000000dd11100d100008110000180000000000650560e
000600000050000000066500060000000000665500000000ccccccccccccc449556cccccc249c94c0000000000f56a000d111111d111111a00000000f0494954
000660000000000000065000000000000000066550000000ccccccccccccc499566ccccccc449ccc000000000000000000d111100dd11110000000000f444440
0000000000000000ddd00000000dd0000000000000000000cccc000455cccccccccc000455cccccc000000000000000000000000000000000000000000000000
dddd0000000dd0000ffddd0000ddbd50dddd000000000000ccc0004555ccccccccc0004555cccccc000000000000000000000000000000000000600000060000
0fffddddddddbd50000fffdddddff5560fffddddddd00000cc4244cc55cccccccc4244cc55cccccc000000000000000000000000000000000000a500006a0000
0000ffffddfff55600000ffffdd00060000ffffffdddd000cc442cc555555ccccc442ccc555ccccc000000000000000000000000000000000000650000650000
00000666dd00006000000666fddd000000000666fddfdd00cc444cc555556cccc444ccccc55ccccc000000000000000000000000600000060007650000655000
00000566660000000000006650dd60000000666fdd0fbd00ccc44cccccc66cccc44cccccc555cccc000000000000000000000000670007a60006550000765000
00000500060000000000000666560000000066550005d000ccc249ccccccccccc249cccccc5556cc000000000000000000000000566766550076500000076000
000005500000000000000000650000000000066550655000ccc499cccccccccccc99ccccccc566cc000000000000000000000000056565500065000000006500
000000000000000000000000000000000000000000000000cccc00005ccccccccccc00005ccccccc000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000cccc0004cccccccccccc0004cccccccc000000000000000000007000000000000000000000000000
000000000000000000000000000000000000000000000000cccc5042ccccccccccc550442ccccccc000000000000000000007070000000000000000000000000
000000000000000000000000000000000000000000000000ccc554444444ccccccc554424ccccccc000000000000000000707e7ee0f000000000000000000000
00000dddddd0000000000ddddd0000000000000000000000ccc555444249cccccc555cc444cccccc0000000000000000007ee8f7874000006700000000766600
0000dfffddddd000000ddffffdddd0000000000000000000ccc555cccc99cccccc55cccc44cccccc000000000000000000e8e7884788f0005667000007655a60
000df666f5dddd000ddfff66dddddd500000000000000000cccc556ccccccccccc556ccc2449cccc0000000000000000007ef78ff88f440005566a6066550000
00ddf665655ffd00dff666655ddff5560000000000000000cccc566cccccccccccc66cccc499cccc00000000000000000ee8484f444844740055550050000000
120000000000111100000000000000111100000000d4b0e4c0e400000000000000000040400000000000004040a093a093b3b3a1300000000000000000000011
01010101c3c300c4010101010101010101000000000101010101b2000000000240b3b3b3b3b3b3b3b370b3b3303030401200000000d403121212121212121212
12000000c2001111e4b20000000000111100000000211010103100b1000000214000000000000000000000a0b3b3b3a1a2a1b330400000000000000000000000
00000001016300007282010101c40101010000000000010101010101000000024040a1b3b3b3b3b3b3b3b3b3b3a0404012000000431212121212121212121212
12000000211011111031000000000011110000000000001111111031000000114040400000000000000000a192a1b33030303040400000000000000000000000
0000000063c3007282000001000000010100000000000063c300000000000002404040b3b3b3b3b3b3b3b370b3b3404012000000000023121212121212121212
12000000000000000000000000000000000000000000000000000000000000114040000000000000000030303030304040404040403000000000000000b2c400
00c2e4b2c3c37282000000000000000101b2e4c2000000c3c30000000000b202404040b3b3b3b3b3b3b3b3b3b3b3934012000000000000000023121212121212
3300000000000000000000b1e4c1000000e4c00000000000000000b200c0e4114040000000000000003040404040404040404040000000000000000000211010
0101010101c38200000000000000b20101010101000000c3c300000000000102404093b3b370b3b3b3b3b3b3b3b3b34012000000000000000000000000002312
00000000000000000000002110101010101010310000000000c20021101010114000000000000000304040404040404040000000000000000000000000000011
010101010101e4c200000000c2e401010172c301010000c363000000000101024040b3b3b3b3b3b370b3b3b3b3b3b34012000000000000000000000000000000
0000000000000000000000000000001111000000000000c0e4211000000000000000000000000000000000000000000000000000000000000000000000000011
01010000c3010101000000000101010101c3820000000063c3000000000000024040b3b3b3b3b3b3b3b3b3b370b3b34012130000000000000000000000e40000
12121300000000000000000000000011110000000000b2211000000000b0e40000e4b00000000000000000000000000000000064000000000000000000000011
0100000063c30000000000000000720101820000000000c3c30000000000000240b3b3b3b3b3b3b3b3b3b3b3b3b3b34012120000000000000000e40003121212
121212121300c1e4b1e4c1000000001111e4b0e4c121101100000000c0211010101031e6e6e6e600e4b0e4c0e400b0e4b2000000e4b2e4c0e4000000c7d70011
01b2e4c2c3c3b2e4c20000000072c3808000e4c1e4b1e4c36360606060e4b1024092b3a1b3b3a2b3b3a1b3a1b3b3b34012121300000000e40003121212121212
12121212121211111111111100000011111010101011111100000021101111111111112020202021101010101010101031000000211010101010101010101011
0101010101010101010100007201010110101010101010103120202020211010403030303030303030303030b3b3b34012129191919112121212121212121212
11111111111111111111111100000011111111111111111100000011111111111111111111111111111111111111111101000000211010101010101010101001
01010101010101010101c472c301010130303030303030303030303030303030303030303030303030303030b3b3b340303093b3b3b330303030303030303030
11110000000000000000000000000000000000000000000000000011110000000000000000000000000000000011111101000000000000000000000000000001
01c383c463c30000000072c38200d40140b393b3a0b3b3b3a0b3b3939393b340409393b3b3b393b3b3a0b3b3b3b3b34040b3b3b3b3b393b3b3a0b3a0b3404040
11000000000000000000000000000000000000000000000000001111000000000000000000000000000000000000111101c2e4b2000000000000000000000001
0173c383c3c30000c272c3820000000140b3b3b3b3b3b3b3b3b3b3b3b3b3b34040b3b3b3b3b3b3b3b3b3b3b3b3b3304040b3b3b3b3b3b3b3b3b3b3b3b3b34040
11000000000000b100c1e4d4b1000000c1e400000000000000111100000000000000000000000000000000000000001101010101c40000000000000000000001
010073c3c36300e4010101010000000140b3b3b3a1b3b3a2a1b3b3b3b3b3b34040b3b370b3b3b3a2b3b3b3b33030b3b3a0b3b3b3b3b3b3b3b3b3b3b3b3b3b340
1100000000001111111111111111111111110000e4b1d400000000000000000000000000000000000000000000000000000101010100000000b2c40000000000
0000007363c30001010101000000000140b3b3b330303030303030b370b3b34040b3b3b3b3b3b33030a1b3b3a0b3b3b3b3b3b370b3b3b3a251a151a151929240
110000000000000011111111111111111111111111111100c100e400b100e4000000000000000000000000000000000000000000000000000101010000000000
c2000000c3c372c38200000000b2e40140b3b3b3b3b3a093b3b3a0b3b3b3b34040b3b3b3b39230404030b3b3b3b3b3b3a1b3b3b3b3b3b3305050505050303040
110000000000000000000000000000000011111111111111111111111111110000c1d4000000000000000000000000000000000000000001018200000000b201
0101c4b2c363c382000000b2e401010140b3a2b3b3b3b3b3b3b3b3b3b3b3b3a093b3b3b3b3304040404030b3a1b3b3303030b3b3b3b3b3404040404040a0b340
11000000000000000000000000000000000000000000111111111111111111111111110000b10000000000000000000000000000000001018200000000c20101
0101010163c38200000000010101010140303030b3b3b3b370b3b3b3b3b3b3b3b3b3b3b33040409393b340303030304040403092b3b33040409393b3b3b3b340
110000000000000000000000000000000000000000000000000000000000711111111111111111e4000000000000000000000000c20101010100000000010101
0101010101c3000000c201010101010140b3b3b3b3b3b3b3b3b3a1b3b3a2b3a1b3b3b3b3b3b393b3b3b3b3b3a0b393404040403030304040b3b3b3b3b3b3b340
11000000000000000000000000000000000000000000000000000000000000000000111111111111110000000000000000000000010101010000000000007301
01018200c3c3000000010173c382010140b3b370b3b3a1b3b3b33030303030303030b3b3b3b3b3b3b3b3b3b3b3b3b34040b3b393b3a0a0b3b3b3b3b3b3b3b340
110000000000b1e4d4000000000000000000000000000000000000000000000000000000000000111100000000000000000000000073c3000000000000000073
c3820000c363000000000000c300010140b3b3b3b33030b3b3b3b3b340404040404030b3b3b3b3b370b3b3b3b370b34040b3b3b3b3b3b3b3b370b3b3b3a13040
110000000000111111c1e6e6e6e6e4b0000000000000000000000000000000006400000000000000111100000000000000000000000063000000000000000000
c300000063c30000000000006300a380a0b3b3a2b34040a2b3b3b3b3b340404040404030b3b3b3b3b3b3a2b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3304040
110000000011111111112020202011110000c1000000000000d4b10000c100e400b1000000c7d7001111c100000000000000b0e4c0e4c3b10060606060c0b0e4
c3000000c3c3e4b0e4c0b0e4c3000080a1b392303040403030b392b3b3928181404040403092b392b3303030a1b392b3b3b3a2b3b3a1a1b3b3b3a23030404040
11000000111111111111111111111111111111110000000011111111111111111111111111111111111111110000002110101010101010103120202020219010
01000000010101010101010180101010303030404040404040303030303081814040404040303030304040403030303030303030303030303030304040404040
cccccccccccccccccccccccccc282282cc282cc22228222228282c282cc22c282cc222282c28228228822822822822882c222282cccccccccccccccccccccccc
ccdccdcccccccccccccccccccc2882882c282c288882888882282c282c2882282c288882cc28228822822888282882282288882cccccccccccccccccccdccdcc
dccdccdccdcccccccccccccccc2882882c288288222c28822c28822822882c288288222ccc2882282282288228228228288222ccccccccccccccccdccdccdccd
cdcdddddddddccccccccccccc288888822888228882cc282c28882288282c2888228882cc28882282288228228228228828882ccccccccccccccdddddddddcdc
cddddd11b35dddccccccccccc2828828828282c22882c282c28282c2882cc28282c22882c28282282c28228882c282c28222882cccccccccccddd53b11dddddc
ddd1ddd5b3b5dddccccccccc288228288288882222882288228888228282c28888222288288888282288228282c2822882222882cccccccccddd5b3b5ddd1ddd
dddd1ddd555ddddddccccccc282cc2c282822828888228888282282882882282282888822822282888882282282288882288882ccccccccdddddd555ddd1dddd
1dd1d1dddddd5d5ddd5ccccc282ccccc282cc28222228222282c28282c28282cc282222282cc282288822882282c2222282222ccccccc5ddd5d5dddddd1d1dd1
ddddddd1ddddddddd5d555cccccccccccc494cc44494cc4cc4994449444494449494ccc49944499449449494444494cccccccccccc555d5ddddddddd1ddddddd
dddddddddd5ddddd5555005ccccccccccc494c499494c494494994499994499994994ccc4999994c4999494999994cccccccccccc5005555ddddd5dddddddddd
dddddfdddddddd520255555ccccccccccc4944994499449499449499449449444c494ccc494444cc499449449944ccccccccccccc555552025ddddddddfddddd
ddd4fffdffdd55d520625676cccccccccc499494c499449494c4949944944944cc494ccc494994ccc494494c494ccccccccccccc676526025d55ddffdfff4ddd
d4ff49ff4445555555267c67ccccccccccc4994c4494949494c49449994c49994c494cc4c4944cccc49994cc494ccccccccccccc76c7625555555444ff94ff4d
ff4ffcccccccc005555556cccccccccccc449494499499949444944944c49944c49944494494444cc49494cc4994cccccccccccccc655555500ccccccccff4ff
4ff4cccccccccccc05555776cccccccccc499499494c494c499944994cc494ccc499999449999994c494494499994ccccccccccc67755550cccccccccccc4ff4
f4cccccccccccccccc05766ccccccccccc494c49494c494cc444c494ccc494cc4944444c4944449949944949444494ccccccccccc66750cccccccccccccccc4f
__gff__
0001110101110204020204020400000001010101010411020202040204000000010111001100040404040402040000000404040404040404040404040400000000000000000080000000000080808080000000000000000000000000000040800000000000000000000000000000404000000000000000000000000008080000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
1100000001010101010101010101010101010101000000000101010101010101101010101010101010101010000000010101010101010101010101010101010113000000140101010101011111111111111111111111111111111111111117171111111111111111111111111111111101010101010101010101010101010101
11000000000000000000000000000011110000000000000000000000000000000000000000000000002000000000002020000000004d0000000000000000000000000000000000000000000000000011110000000000000000000000000000172100000000000000000000000000002020000000000000003710100000000011
114c000000000000000000000000001111000000000c0c000000000000000000000000000000000000200000002b002020004e00000000000000004d1c4e00001b001c4d0000000000000000001c4e1111001c4e4d1b0000000000000000001121310000000000000000004e0b0c4d2020000000000000000037103800000011
110113000c000000000000000000000000000000001213000000000000000012130000000000002b0020000000101010101010002b000000000000111111111711111111000000000000000000111111111111111111000000000000000000112121310000000000000000101010102020004e0b4e0c4d000000101010000011
111111011300000000000000004d000b00000b4e001111004e0000000c0000111100000000001010102000000000002020101010100000000000000000001111110000000000001c4d2b000000000011114d000000000000001c4e0000000011212121314d4e0000000000000000002020101010101010000000000000000011
1111111111404100000000000012010101010101011111011300000012010111110c0000000000000020002c00000020200000000000004d00002c000000001111000000001b4e1111114e1b0000001111000000000000000011111b4d1c4e1121212121212131004e4d00000000000000000000000000000000000000000011
11111111110113000000000000000011110000000000000000000000000000111101130000000000001010100000002020000000002b001010101000000000111100000000111111111111110000000000000000000000004e111111111111112121212121212121212135000000000000000000000000000000004d1b1c4e11
110000000000000000000b0000000011110000000000000000000000000000171111110b0000000000000000000000202000000000101010101000000000001111000000000000000000000000000000000000000000001c1111000000000021212133000000000000000000000000004d000000000000000000001201010111
110000000000004d0c121300000000111100000000000000000000000000001711111101011300000000000000000020204e2c0000003c1010000000004e0011114e1c4e1b000000000000004e1b1c4d001c0000000000111100000000000021212100000000000000000000004e302121350000000000000000000000000011
1100000000001201011111000000001111000000000000000000000000000017170000000000000000000000000010101010100000003c100000004e1b12011111111111111100000000000011111111111100000000000000000000004e302121330000000000000000004e30212121210000000000004e4d00000000000011
11000000000000000000000000000000000000000000002b0000000b4e000011110000000000000000000000101010101000000000003c00000000120111110000000000000000000000000000000011110000000000000000000000342121213300000000000000004e30212121212121000000000000111100000000000011
00000000000000000000000000000000000000000000001000000012010101111100000000000000000000000000463c3c00000b4e0c3c000000000000000000000000000000000000000000000000000000000000004e1c0000000000000000000000000000004e302121212121212121000000000030111131000000000000
0c4e0b4e0c4e0b4e0c4e0b000000000c0b0c4e0c0b6e6e266e6e6e111111111111007c7d00001b0c2b000b001c00003c3c0c4e120101134e0b000c4e000b4e0000000b000000001c4e2c4e1c1b00000000001b0000001111114e00004e00000000004e00000030212121212121212121210000004d3021212121314e000c0b00
0101010101010101010113000012010101010101010202020202021111111111110101010101010101010101010101010101011111111109090101010101010101010110080810111111111111111111111111000000111111112121212121212121213500002121212121212121212121000034212121212121212101010101
2121212121212121212133000012010101010101010101010101010101010113000000000000000000000000000000000000000000000000000000000000000410101008080808101010101010101010101010000000120101010101010101012121210000002121212121212121212133000000322121212121212121212121
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004200000003c3c0000000000003c000000000000000000000000000000000000112121330000343300000000000000000000000000000032212121330000000000
004c0000000000000000004e0c4c000000000000000000000000000000004c000000000000000000000000000000000000000000000000000000000000000004200000003c3c002c0000000036000000000000000000000000000000000000112133000000000000000000000000000000004e4e0000000000000000004e004e
21213500000000000b00302121310000000c4e000000000b00000000000012010300000000000000000000000000000000000000000000000000000000000404200000003c36080800002c003c00000000004d002c00000000000000000000112100000000000000000000000030212121212121310000000000004d30212121
213300000000003421212121211101010101130000001213000000000000001104034d00000000004c2c4c00000000000000000000000000000000000000040420000000360808000000080808001010101010101010000000000000002c00112100000000000000000000302121212121212121213100000000342121212121
21000000000000000000000000000011110000000000111100000000000000110404030000000000030303000000000000000000004c00000000000000040404101000003c3c0000000000003c000020101010000000000000000000120909112131000000000000302121212121212133003221212131000000002121212121
210c4e4c0b0000000000000000000011110000000c4c1111000c000000000011040404032b000000000000000000000000000000000303000000000000040404200000003c3c00000000000036000020100000000000000000000000000000112121316e6e6e3021212121330000000000000032212121314e00003221212121
21212121212131000000000000000011110000001201111101130000000000110404040403000000000000000000000000000000000004032c00000000040404200000003c36002c000000003c000020110000000000000000000000000000112121212222222121213300000000000000000000322121212135000032212121
212121212121212135000000000000111100000000000000000000000c1b0011040404040400000000000000000000000000002c00000004030000000004040420000000363c08080000000036101010110000002b004d4e000000000000000000000000322121330000000000000000004d4e00000000000000000000322121
212121330000000000000b4c000000111100000000000000000000001201011104040404040300000000000000002c000000000303000004040000000000000420002c08080808000000002c3c0000201100000012010113000000000000000000000000000000000000003421212121212121350000000000004e0000003221
00000000000000000000121300000000000000000000000000000000000000110404040000000000000000000000034c00000000000000000403000000000000000008083c3c0000000000083600002011000000001111000000000000000000000000000000000000000000003221212133000000004e4d3021213500000000
000000000000000000001111000000000000000000000000000000000000001104000000000000000000000000000403032c0000000000000004030300000000000000003c360000000000003c000020110000000011110000000000000030212131460000000000000000000000322121000000302121212121330000000000
0000000000004e0b0c4e11114e0c000b00004e1c000000000000004e0b0c4e00004e2c000000000000000000004c04040403000000000000000404040000000000000000363c00000000000036000020114e4e4e4e1111000000000c4e3021212121310000000000000000007c7d3021190030212121212121330000004e4d00
21213500001201010101111101010101010101130000000012010101010101010403030000000000000000000003040404040300000000000004040403030303101000003c3c0000000000003c001010010101010111110000001201212121212121211919191921212121212121211919192121212121212100000034212121
21330000000101010101010101010101010101130000000012010101010101010404040000000000000000000004040404040400000000000004040404040404101000003c3c00000000000008081010101010101010100000001010101010100303030a3b3b3b03030303030303181819212121212121213300000000212121
214d0000000000000000000000000000000000000000000000000000000011110404040000000000000000000004040404040403000000000000000000000011100000003c360000000000000000000000000000000000000000000808080810043b0a3b3b3b3b3b3b0a3b0a3b0a3b1821212121212133000000004d30212121
2100000000000b0c000000004d000000004d2c0000000000000000000000001104040403002c00000000002c0004040404043b0a00000000000000000000001110002c00363c2c2b002b2c0000000000000000000000002b4c2c000000080810043b3b3b3b3b3b3b3b3b3b3b3b3b3b042121213300000000004e302121212121
210000000000121300000000120101010101130000000000000000000000000000040404030300000000000303040404040a3b3b0000000000000000000000111010102b3c3c1010101010002b2c2b0000000000004e2b101010000000000810043b3b3b073b3b3b3b3b3b3b3b1a2904213300000000004e3021212121212121
__sfx__
000200000b4330943109431094330a43110431164331a4311d4311e43320431204311e43317431164331643114431134331343115433134011340113401134011240110401014012940127401254012240123401
011200003b7503b7503b750007003b7503b7503b750007000070036750387503b7503b75038750367503675031750317503175000700317503175031750007000070000700007000070000700007003675038750
011200003874038740387400000038740387403874000000000000000000000000000000000000000000000034750347503475000000347503475034750000000000000000000000000000000000000000000000
011200003472034720347200000034720347203472000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011200001c5102351025510235101c5102351025510235101c5102351025510235101c51023510255102351021510285102a5102851021510285102a5102851021510285102a5102851021510285102a51028510
011200003b7503b7503b750007003b7503b7503b750007003670036750387503b7503b75038750367500070034750347503475000700347503475034750007000070036750387503b7503b750387503675036750
011200003674036740367400000036740367403674000000000000000000000000000000000000000000000031750317503175000000317503175031750000000000000000000000000000000000000000000000
011200003372033720337200000033720337203372000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01120000235102a5102c5102a510235102a5102c5102a510235102a5102c5102a51020510285102a5102851020510285102a5102851020510285102a51028510235102a510215102a5102c510235102a51023510
01120000383553b3553b3053b3553b3053b3553b3053b355383553b3553b3553b3053b3553b305383553835500305383553b3553b3053b3553b305383553b3553d3553b355003053b3553b354003050030500305
011200003b7103b7103b7103b7103b7103b7103b7103b7103b7103b7103b7103b7103971039710397103971038710387103871038710387103871038710387103871038710387103871038710387103871038710
011200003871038710387103871038710387103871038710387103871038710387103671036710367103671034710347103471034710347103471034710347103471034710347103471034710347103471034710
011200003b7103b7103b7103b7103b7103b7103b7103b7103b7103b7103b7103b7103971039710397103971038710387103871038710387103871038710387103b7103b7103b7103b71039710397103b7103b710
011200003871038710387103871038710387103871038710387103871038710387103671036710367103671034710347103471034710347103471034710347103471034710347103471034710347103471034710
0112000038355393553b3053b3553b3553b3053b3553b35438355383553b3553b3553b3053b3550030500305383553b3553b3053b3553b3053b3553b3053b3553d3553d3553b3553b3053b3553b3540030500305
01120000003053d355393550030539355003053935539355363553935500305393553d3553d3553b3553b355003050030500305003053b3553b355003053b3553d3553d3553b355003053b3553b3550030500305
01120000003053930539355393053935539355363553435500305363053930536355393550030539355003053d3553b3553b3053b3553b3053b3553b3053b3553d3553b3553b3053b3553b3553b3053b3053b304
011200003d3553b3553d3553b3553b3053b3753b350383553b3553b35538355003050030534355383553b3553d3553b3553b350383553d3543b3753b355393503935539350003050030500305393553b3553b355
011200003b3553b35500305003050030500305003050030536305363553635536355363553635436350363503435534355383050030500305003050030538355383502f355313552f35538355383553835034355
0112000034350343501a70024700287002b7002a7003b7003b7003e7003b7003b7003b70000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000100000d5730d5730c5730c573335733357333573365733a5733d5733e5733f5733f5733f573005030050300503005030050300503005030050300503005030050300503005030050300503005030050300503
000100001757314573135731557324573275732557325573285732f57333573355733857337573345733457334573355733657300503005030050300503005030050300503005030050300503005030050300503
000200003b6223a6123a6123a6223b622396323c6123f622074730747307473074730747308473094730a4730b4730d4730f4730b4730b4730b47310473154731747317473174731547315473134730f4730b473
00020000086520a6320b622116321a6322563232642396423d6423e6423f6423f6423f6323f6223f6123f6123f6123f6123f6123f6123f6023f6023f6023f6023f6023f6023f6023f6023f6023f6023f6023f602
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
01 01020304
00 05060708
00 090a0b04
00 0e0a0b08
00 0f0a0b04
00 100a0b08
00 110a0b04
00 120a0b08
02 130a0b04
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
