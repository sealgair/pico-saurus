pico-8 cartridge // http://www.pico-8.com
version 8
__lua__
-- debug settings
showstats=false

-- game states:
gs={
 gameover=-1,
 init=0,
 play=1,
 slide=2,
 sleep=3,
}

gamestate=gs.init

-- sprite flags
sflags={
 sm=0, -- solid map
 mf=1, -- map foreground
 mb=2, -- map background
 cn=3, -- carrion
 wm=4, -- water map tile
 fs=6, -- edible fish spawn
 cs=7, -- edible critter spawn
}
dt=1/60 --clock tick time
g=9.8*60 -- gravity acceleration
day=60 -- day length in seconds
twilight=6 -- twilight length
maxnum=32767.99

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
  if n>0 then return t[n] end
 end
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

-- return a table containing numbers from s (def 1) to e
function range(e, s, d)
 local s=s or 1
 local d=d or 1
 local r={}
 for i=s,e,d do
  add(r,i)
 end
 return r
end

-- randomly choose item from table
function rndchoice(t)
 return t[flr(rnd(#t))+1]
end

-- convert sprite flags to a map layer
function mlayer(...)
 l = 0
 for f in all({...}) do
  l+=2^f
 end
 return l
end

function drawstats()
 if showstats then
  rectfill(0,120, 127,127, 5)
  local m="#"..flr(stat(0))
  m=m.."\t%"..flr(stat(1)*100)
  m=m.."\ta:"..#world.actors
  m=m.."\tvx:"..flr(protagonist.vel.x)
  print(m, 2,122, 10)
 end
end

-- useful for iterating between x/y & w/h
xywh={x='w', y='h'}

-- is color a darker than color b?
darkorder={
 0,1,2,5,4,3,8,13,9,14,6,11,15,12,10,7
}
darkindex={}
for k,v in pairs(darkorder) do
 darkindex[v]=k
end
function darker(a, b)
 return darkindex[a]<darkindex[b]
end

-- color mappings for nighttime
nightmap={
 [1]=0,
 [2]=1,
 [3]=1,
 [4]=5,
 [5]=0,
 [6]=5,
 [7]=6,
 [8]=4,
 [9]=4,
 [10]=9,
 [11]=3,
 [12]=13,
 [13]=2,
 [14]=8,
 [15]=4,
}
function isnight()
 return daytime>day-twilight/2 and daytime<day*2-twilight/2
end
function mapnight()
 if isnight() then
  for f,t in pairs(nightmap) do
   pal(f, t)
  end
 end
end

-- print with color
function cprint(msg, c)
 color(c)
 print(msg)
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

-- concatenate two list-like tables
function concat(a, b)
 local r={}
 for v in all(a) do
  add(r, v)
 end
 for v in all(b) do
  add(r, v)
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

notenames={
 c=0,
 cs=1,
 d=2,
 ds=3,
 e=4,
 f=5,
 fs=6,
 g=7,
 gs=8,
 a=9,
 as=10,
 b=11,
}

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

function getmusicsounds(m)
 local music=getmusic(m)
 if sounds==nil then
  sounds={}
 end
 for i=0,3 do
  local s=band(peek(music.b+i),127)
  if s!=0x42 then
   add(sounds, s)
  end
 end
 if not music.loopback and not music.stop then
  sounds = concat(a, getmusicsounds(m+1))
 end
 return sounds
end

function setmusic(m, args)
 -- TODO
end

function getsound(s)
 local l=68
 local b=0x3200+s*l
 return {
  speed=peek(b+65),
  --TODO: loop start / end
 }
end

function setsound(s, args)
 local l=68
 local b=0x3200+s*l
 if args.speed!=nil then
  poke(b+65, args.speed)
 end
 --TODO: loop start / end
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
 -- TODO: instrument, effect
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

--------------------------------
-- particles
--------------------------------

partgen = class()

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
}

function actor:init(x,y)
 self.x=x
 self.y=y
 self.vel={x=0,y=0}
 self.acc={x=0,y=0}
 self.flipped=false
 self.upsidedown=false
 self.grounded=false
 self.walled=false
 self.wfp=0 --current pixel of walking animation
 self.wfd=8 --number of pixels per frame of walking animation
 self.health=4
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
  x=self.x+self.w*8/2,
  y=self.y+self.h*8/2,
 }
end

function actor:touch(block)
end

function actor:move()
 --accelerate
 self.vel.x += self.acc.x*dt
 self.vel.y += self.acc.y*dt
 self.vel.y += g*dt

 --upgade graphical stuff :p
 if self.vel.x==0 then
  self.wfp=0
 elseif self.sprites.walk then
  self.wfp=wrap(
   self.wfp+abs(self.vel.x*dt),
   self.wfd*#self.sprites.walk
  )
 end

 --update coords
 local newx=self.x+self.vel.x*dt
 local newy=self.y+self.vel.y*dt

 local hb=self:hitbox()

 --check for map collisions (x)
 local dx=-1
 if (self.vel.x>0) dx=hb.w-1
 self.walled=false
 for x=hb.x,newx,sign(self.vel.x) do
  local c=world:collides(x+dx, hb.y, 1, hb.h-1)
  if c then
   newx=x
   self.vel.x=0
   self.walled=true
   self:touch(c)
   break
  end
 end
 self.x=newx

 --check for map collisions (y)
 local dy=-1
 if (self.vel.y>0) dy=hb.h-1
 self.grounded=false
 for y=self.y,newy,sign(self.vel.y) do
  local c=world:collides(hb.x, y+dy, hb.w-1, 1)
  if c then
   self.grounded=true
   newy=y
   self.vel.y=0
   self:touch(c)
   break
  end
 end
 self.y=newy
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

function actor:draw(o)
 spr(
  self:sprite(),
  self.x, self.y,
  self.w, self.h,
  self.flipped,
  self.upsidedown
 )
end

-- a hook to perform actions when an actor gets hurt
-- calls despawn if health runs out
function actor:hurt(d, keep)
 self.health-=d
 if self.health<=0 then
  self.health=0
  if not keep then
   world:despawn(self)
  end
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
  jump={94, 95},
  flop={110, 111},
  pinned=110,
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
 world:particles{
  x=self.x+4, y=self.y+4,
  colors={7,12,1,13},
  duration=0.1,
  rate=20,
 }
end

function fish:touch(s)
 if fget(s, sflags.wm) then
  world:despawn(self)
  self:splash()
 elseif fget(s, sflags.sm) then
  self.vel.x=0
 end
end

function fish:move()
 actor.move(self)
 if (not self.grounded
   and not protagonist.grounded
   and actor.hitbox(self):overlaps(protagonist:hitbox())) then
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
  walk={78},
  pinned=79,
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
 if self.pinned then
  return self.sprites.pinned
 end
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

 if self.walled then
  self.vel.x*=-1
 end
 if self.vel.x!=0 then
  self.flipped=self.vel.x<0
 end
end

--------------------------------
-- rahonavis

rahonavis = critter.subclass{
 __name="rahonavis",
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
end

function rahonavis:sprite()
 if self.eating then
  return self.sprites.eat
 elseif not self.grounded then
  return self.sprites.jump
 else
  return critter.sprite(self)
 end
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
 if self.rethink==nil then
  self.rethink=rnd(3)
  self.direction*=-1
 end
 self.rethink-=dt
 if self.rethink<=0 then
  self.rethink=nil
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
   if actor.type!=rahonavis and actor:overlaps(self) then
    self.eating=true
    actor.pinned=true
    self.x=actor.x
    actor:munch(dt)
    if actor.health<=0 then
     self.direction=rndchoice({1,-1})
    end

    if self.eatparts==nil then
     local args=self:mouth()
     args.colors={8,8,14}
     args.rate=20
     self.eatparts=world:particles(args)
    end
    break
   end
  end
 end
 if not self.eating and self.eatparts then
  self.eatparts:stop()
  self.eatparts=nil
 end

 if self.eatparts then
  local m=self:mouth()
  self.eatparts.x=m.x
  self.eatparts.y=m.y
 end
end

--------------------------------
-- majungasaurus

majungasaurus = critter.subclass{
 __name="rahonavis",
 critter=false,
 afraid=false,
 run={m=40},
 sprites={
  body=70,
  stand=87,
  walk={87,103,87,119},
 },
 w=4,
 h=2,
}

function majungasaurus:init(...)
 critter.init(self, ...)
 local tb=world:tilebox()
 self.carrion=0
 local i=0
 for x=tb.l,tb.r do
  for y=tb.t,tb.b do
    local s=mget(x, y)
    if fget(s,sflags.cn) then
     self.carrion+=x*8
     i+=1
    end
  end
 end
 self.carrion/=i
end

function majungasaurus:think()
 local hb=self:hitbox()
 if hb:contains({x=self.carrion,y=self.y+1}) then
  self.vel.x=0
 else
  self.vel.x=self.run.m*sign(self.carrion-hb.x)
 end
end

function majungasaurus:move()
 critter.move(self)
 if self:overlaps(protagonist) then
  local mm=self:middle()
  local pm=protagonist:middle()
  local dx=mm.x-pm.x

  if protagonist.y>self.y+8 then
   if self.flipped==(dx>0) then
    protagonist.vel.x=-sign(dx)*200
    protagonist.vel.y-=80
    protagonist:munch(3)
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

 spr(
  self.sprites.body,
  self.x, self.y, 4,1,
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
 self.j=0
 self.eating=false
 self.drinking=false
 self.crouched=true
 self.es=1
 self.esd=0
 self.efc=8
 self.food={}
 self.stats={
  health=1,
  food=.8,
  water=.9,
  sleep=1,
 }
 self.btndelay={}
 self.score={
  food=0,
  water=0,
  sleep=0,
  mammals=0,
  rahonavii=0,
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
 if self.health<=0 then
  return self.sprites.dead
 elseif self.sleeping then
  return self.sprites.sleep
 elseif self.crouched then
  if self.sleepcount and self.sleepcount>1.2 then
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
 return world:collides(m.x, m.y+1, 1, 1, sflags.wm)
end

-- coords of my mouth
function player:mouth()
 local c=self:middle()
 c.y=self:hitbox().b
 if self.flipped then
  c.x-=4
 else
  c.x+=4
 end
 return c
end

-- delayed button detection
function player:dbtn(b)
 local d=bound(.33-self.stats.sleep, 0.33, 0)*1.5
 local r=false
 if self.btndelay[b]==nil then
  self.btndelay[b]=0
 end
 if btn(b) then
  r=self.btndelay[b]>=d
  self.btndelay[b]+=dt
 else
  self.btndelay[b]=0
 end
 return r
end

function player:move()
 if self.sleeping then return end
 local slpadj=bound(self.stats.sleep*2, 1, 0.3)

 if btn(self.btn.l) then
  self.flipped=true
 elseif btn(self.btn.r) then
  self.flipped=false
 end
 if self:dbtn(self.btn.j) and self.grounded then
  self.j=min(self.j+self.jd,1)
 elseif self.j>0 then
  self.vel.y-=self.jump*(1+self.j)*slpadj
  self.j=0
 end

 local run=self.run.m*slpadj
 if btn(self.btn.s) then
  run*=1.5
 end

 self.crouched=btn(self.btn.c)
 if self.crouched then
  if self.sleepcount==nil then
   self.sleepcount=0
  elseif not self:onwater() then
   self.sleepcount+=dt
   if self.sleepcount>3 then
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

 if not self.crouched then
  self.sleepcount=0
 end

 -- drink/eat
 self.eating=false
 self.drinking=false
 if self.grounded and self:dbtn(self.btn.e) then
  local m=self:mouth()
  if #self.food>0 then
   self.eating=true
   self:eat()
  elseif world:actorcollides(self, sflags.cn) then
   self.eating=true
   self.stats.food+=dt*.05
   self.stats.water+=dt*.025
  elseif self:onwater() and abs(self.vel.x)<dt then
   self.drinking=true
   self:drink()
  end
 end
 if self.eating or self.drinking then
  self.vel.x=0
  self.acc.x=0
 end
 if not self.eating then
  self.es=1
  self.esd=0
 end

 if self.eating and not self.eatparts then
  local args=self:mouth()
  args.colors={8,8,14}
  args.rate=50
  self.eatparts=world:particles(args)
 elseif not self.eating and self.eatparts then
  self.eatparts:stop()
  self.eatparts=nil
 end
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
 if self.vel.x!=0 and #self.food>0 then
  for f in all(self.food) do
   if not self:overlaps(f) then
    f.pinned=false
    del(self.food, f)
   end
  end
 end
end

-- reset sleep counter if player gets hurt
function player:hurt(d)
 actor.hurt(self, d, true)
 self.sleepcount=0
 if self.sleeptime!=nil then
  self.sleeptime+=d*10
 end
end

-- decrement stats
function player:age(dt)
 local d=(1/day)*dt
 local s=abs(self.vel.x)/self.run.m
 self.stats.water-=d/3
 self.stats.food-=d/5*(.6+s)
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

function player:drink()
 self.stats.water+=dt/20
 self.score.water+=dt/20
end

function player:eat()
 local f=self.food[1]
 local a=f:munch(8*dt)/100
 self.stats.food+=a
 self.stats.water+=a/2
 self.score.food+=a
 self.score.water+=a/2
 f.x=self:mouth().x-f.w*4
 f.y=self.y
 f.flipped=self.flipped
 if f.health<=0 then
  del(self.food, f)
  if f.type==critter then
   self.score.mammals+=1
  elseif f.type==fish then
   self.score.fish+=1
  elseif f.type==rahonavis then
   self.score.rahonavii+=1
  end
 end
end

function player:findfood(actors)
 if self.vel.y<=0 then return end
 for a in all(actors) do
  if a.critter and self:overlaps(a) then
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
 if isnight() then
  self.stats.sleep+=d/5
 end

 local hd=1/3
 hd+=min(self.stats.food*2-1, 0)
 hd+=min(self.stats.food*2-1, 0)/3
 self.stats.health+=d*hd
 for k,v in pairs(self.stats) do
  self.stats[k]=min(max(v,0),1)
 end

 -- check whether we woke up
 if (self.stats.sleep>=1
   or self.sleeptime>day*2/3
   or daytime<(day/60)) then
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
  w=3,h=2,
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
 spawns={
  critters={},
  fish={},
 },
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
   c=colors[flr(rnd(#colors)+1)]
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
  if actor.type==critter then
   local s=self:screenkey()
   self.critterpop[s]-=1
  end
  del(self.actors, actor)
 end
 for pg in all(self.partgens) do
  if pg.actor==actor then
   pg:stop()
  end
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

-- spawn all visible critters
function world:findspawns()
 -- should this screen have carrion
 local s=self:screenkey()
 if self.carrion[s]==nil then
  self.carrion[s]=rnd()>0.05
 end

 for a in all(self.actors) do
  if a.critter then
   local b=world:checkbounds(a:middle())
   if b.x+b.y!=0 then
    del(world.actors, a)
   end
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
   local s=mget(x,y)
   if fget(s,sflags.cs) then
    if majungasaurus:spriteset()[s] then
     if self.carrion[self:screenkey()] then
      add(self.spawns.apex,
       {x=x*self.pixels.w, y=y*self.pixels.h, type=majungasaurus})
     end
    elseif rahonavis:spriteset()[s] then
     add(self.spawns.danger,
      {x=x*self.pixels.w, y=y*self.pixels.h, type=rahonavis})
    else
     add(self.spawns.critters,
      critter(x*self.pixels.w, y*self.pixels.h))
    end
   elseif fget(s,sflags.fs) then
    add(self.spawns.fish,
     {x=x*self.pixels.w, y=y*self.pixels.h})
   end
  end
 end

 self:spawn_critters()
end

function world:spawn_critters()
 -- choose from available based on population
 local s=self:screenkey()
 local cn=#self.spawns.critters
 if self.critterpop[s]==nil or self.critterpop[s]>cn then
  self.critterpop[s]=3
 end
 local ids=range(#self.spawns.critters)
 for i=1,self.critterpop[s] do
  local ci=rndchoice(ids)
  del(ids, ci)
  local c=self.spawns.critters[ci]
  self:spawn(c)
 end
 if rnd()>.5 then
  self:spawn_danger()
 end
end

function world:spawn_danger()
 local c=rndchoice(self.spawns.danger)
 if c then
  self:spawn(c.type(c.x, c.y))
 end
 if #self.spawns.apex>0 then
  local hasapex=false
  for a in all(self.actors) do
   c=rndchoice(self.spawns.apex)
   if a.type==c.type then
    hasapex=true
    break
   end
  end
  if not hasapex then
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

function world:hasmajung()
 for a in all(self.actors) do
  if a.type==majungasaurus then
   return true
  end
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
   if s.pre==nil then s.pre=0  end
   s.pre-=dt
   if s.pre<=0 then
    s.pre=nil
    self:spawn(fish(s.x, s.y))
    del(self.prespawn, s)
   end
  end
 end

 if self.nextdanger==nil then
  self.nextdanger=3+rnd(8)
 end
 self.nextdanger-=dt
 if self.nextdanger<=0 then
  self.nextdanger=nil
  self:spawn_danger()
 end
 if gamestate!=gs.sleep then
  self.nextfish-=dt
  if self.nextfish<=0 then
   self.nextfish=rnd(7)
   self:spawn_fish()
  end
 end

 if self.hadmajung!=self:hasmajung() then
  self.hadmajung=not self.hadmajung
  reloadmusic(0)
  if self.hadmajung then
   minorize(0, notenames.e)
   settempo(0, 16)
  end
 end

 world:move_actors()
end

function world:move_actors()
 for a in all(self.actors) do
  a:move()
  local b=self:checkbounds(a:middle())
  if b.x!=0 or b.y!=0 then
   if a==protagonist then
    gamestate=gs.slide
    self:translate(b)
   else
    if fading[a] == nil then
     fading[a]=1
     if gamestate==gs.sleep then
      fading[a]=0
     end
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
  self.critterpop[s]=p+2
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

-- check for collisions in actor
function world:actorcollides(a, flag)
 local hb=a:hitbox()
 return self:collides(hb.x, hb.y, hb.w, hb.h, flag)
end

-- check for collisions in box
function world:collides(x,y,w,h, flag)
 flag = flag or sflags.sm
 for nx=x,x+w do
  for ny=y,y+h do
   local s=mget(flr(nx/8),flr(ny/8))
   if fget(s,flag) then
    return s
   end
  end
 end
 return false
end

-- check whether point is outside bounds
function world:checkbounds(p)
 local b=self:pixelbox()
 b.l=b.x
 b.r=b.x+b.w
 b.t=b.y
 b.b=b.y+b.h
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
   if self.screens[k]<0 or self.screens[k]>=self.screens[s] then
   end

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
 local cs={12,13,2,1,0}
 local cn=#cs-2
 local ci=1
 if daytime>day then
  if daytime>=day*2-twilight then
   ci+=flr((day*2-daytime)*cn/twilight)
  else
   ci=#cs
  end
 elseif daytime>=day-twilight then
  ci+=cn-flr((day-daytime)*cn/twilight)
 end
 rectfill(0,0,127,127,cs[ci])
 if ci>1 then
  for s in all(self.stars) do
   if darker(cs[ci], s.c) then
    pset(s.x, s.y, s.c)
   end
  end
 end
end

function world:print(msg, x, y, c)
 local o=self:offset()
 x-=o.x
 y-=o.y
 print(msg, x,y, c)
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
  mapnight()
  camera(pb.x-self.o.x, pb.y-self.o.y)
  map(tb.x,tb.y, pb.x,pb.y, tb.w,tb.h, layer)
  if wrapping then
   map(
    w.c.x, w.c.y,
    pb.x+w.p.x, pb.y+w.p.y,
    w.c.w,w.c.h,
    layer
   )
  end
  pal()
 end

 -- do the actual drawing
 local bgl
 if not wrapping and self.carrion[self:screenkey()] then
  bgl=mlayer(sflags.mb, sflags.sm, sflags.cn)
 else
  bgl=mlayer(sflags.mb, sflags.sm)
 end
 drawmaps(bgl) -- background
 for a in reverse(self.actors) do
  a:draw()
 end
 for p in all(self.partgens) do
  p:draw(self:offset())
 end
 drawmaps(mlayer(sflags.mf)) -- foreground
 camera()
end

--------------------------------
-- the hud
--------------------------------

function drawahud(s, bc, x,y, m)
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
 --heart
 drawahud(13,8, 0,0, protagonist.stats.health)
 --stomach
 drawahud(29,13, 0,8, protagonist.stats.food)
 --water
 drawahud(45,12, 64,0, protagonist.stats.water)
 --sleep
 drawahud(61,7, 64,8, protagonist.stats.sleep)
 pal()
end

--------------------------------
-- the game
--------------------------------

function _init()
 protagonist=world:spawn_protagonist()
 world:findspawns()
 world:makestars(128)
 daytime=0
 wakingtime=0
 sleeptime=0

 fading={}
end

-- so that the web player works
function _update()
 _update60()
 _update60()
end

wasnight=false
function _update60()
 if gamestate==gs.gameover then
  gotime+=dt
  if gotime>8 and (btnp(4) or btnp(5)) then
   run()
  end
  return
 elseif gamestate==gs.init then
  if btnp(4) or btnp(5) then
   gamestate=gs.play
   music(0)
  else
   return
  end
 elseif gamestate==gs.play then
  world:advance(dt)
 elseif gamestate==gs.slide then
  if world:translate() then
   gamestate=gs.play
   world:findspawns()
  else
   return
  end
 elseif gamestate==gs.sleep then
  local sdt=dt*10
  sleeptime+=dt
  world:advance(sdt)
  if protagonist:snooze(sdt) then
   gamestate=gs.play
   reloadmusic(0)
   sleeptime=0
   wakingtime=.5
  end
  return
 end
 if wakingtime > 0 then
  wakingtime-=dt
 end
 if isnight()!=wasnight then
  wasnight=isnight()
 end

 protagonist:findfood(world.actors)
 protagonist:age(dt)

 if protagonist.stats.health<=0 then
  gamestate=gs.gameover
  gotime=0
 elseif protagonist.sleeping then
  gamestate=gs.sleep
  world:startsleep()
  reloadmusic(0)
  altvolume(0, .8)
 end
end

function drawsplash()
 rectfill(16,16,111,57,5)
 cursor(19,19)
 cprint("masiakasaurus knopfleri", 8)
 cprint(" mark knopfler's", 9)
 cprint(" vicious lizard", 9)
 cprint(" x to jump", 6)
 cprint(" hold z to run or eat", 6)
 cprint(" x or z to start", 7)
end

function drawgameover()
 local p=protagonist:middle()
 local o=world:offset()
 local l=2
 p.x-=o.x
 p.y-=o.y-2
 local d=(1-min(gotime/l, 1))*127

 color(0)
 rectfill(0,0,127,p.y-d)
 rectfill(0,p.y+d,127,127)

 camera(o.x, o.y)
 protagonist:draw()
 camera()
 if d<=0 then
  d=(min(gotime/l-1, 1))*max(p.x, 127-p.x)
  if d>8 then
   clip(p.x-d, 0, d*2, 127)
   line(0, p.y+1, 127, p.y+1, 8)
   local tx=p.x+16
   if p.x>64 then
    tx-=64
   end
   print("game over", tx, p.y-4, 8)
   clip()
  else
   line(p.x-d, p.y+1, p.x+d, p.y+1, 8)
  end
 end

 if gotime>l*2 then
  local txt={
   "days survived: "..protagonist.score.days,
   "slept: "..flr(protagonist.score.sleep),
   "drank: "..flr(protagonist.score.food*100),
   "ate: "..flr(protagonist.score.food*100),
   " - "..protagonist.score.mammals.." mammals",
   " - "..protagonist.score.fish.." fish",
   " - "..protagonist.score.rahonavii.." rahonavii",
   "",
   "x or z to restart",
  }
  local f=(gotime-l*2)*10
  local i=0
  cursor(8,8)
  color(4)
  for row in all(txt) do
   if i+#row<f then
    print(row)
    i+=#row
   else
    print(sub(row, 0, f-(i+#row)))
    break
   end
  end
 end
end

function drawsleep(time)
 local c=5
 if (isnight()) c=6
 local p=protagonist:middle()
 local n=3
 if wakingtime > 0 then
  n=flr(n*2*wakingtime)
 end
 for i=0,n-1 do
  x=p.x-6+i*4
  local y=p.y-10+sin((sleeptime+dt*4*i))*3
  world:print("z", x, y, 5)
 end
end

function _draw()
 world:draw({x=0, y=16})
 drawhud()
 if gamestate==gs.init then
  drawsplash()
 end
 if gamestate==gs.sleep then
  drawsleep(min(sleeptime/2, .8))
 elseif wakingtime > 0 then
  drawsleep(wakingtime)
 end
 if gamestate==gs.gameover then
  drawgameover()
 end
 drawstats()
end
__gfx__
700000073333b3331111111100000000000000000000000000000000000000000000000000000000000000000bb0bbb00333033055ee2ee55555555555555555
070000703b3b33b3cdc7cc7c0000000000000000000000000000000000000000000000000000000000000000b00b0000000030035e70e07e5ec55ec55ec55555
0070070093b339391111ccc10000000000000000000000000000000000000000000000000000000000000000000490bb330440005700000ee7eee7eee7eeee55
00077000993399947ccc1d1c000000000000000000000000000000000000000000000000000000000000000000099900004440005e00000000e0000000e000e5
0007700043934494c1c11ccc0000000000000000000000000000000000000000000000000000000000000000b00940000004f0035700000000000e00000000e5
00700700944994439c39c9c900000000000000000000000000000000000000000000000000000000000000000b0990000004403052e000eeee7eee7eee7eee55
070000704344434949943493000000000000000000000000000000000000000000000000000000000000000000999000000f4400557e0e7555ec55ec55ec5555
700000074444444444444444000000000000000000000000000000000000000000000000000000000000000000049000000440005552e2555555555555555555
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000054f555555555555555555555
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005f0ffff54ff54ff54ff55555
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005f00f00ff00ff00ff00fff45
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005f000000f000f000f000f0f5
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005f0000f000f000f000f000f5
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000054f00ff00ff00ff00ff00f45
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000554ff54ff54ff54ff54ff555
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000555555555555555555555555
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000551111555555555555555555
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000510110155111555551115555
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000510000111000155110001555
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000510000100000011000000155
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000510000000110000001100015
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000510000011551000115510015
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000551001155555111555551155
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000555115555555555555555555
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000565665555555555555555555
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000556006566556655666555555
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000650000600660066000666666
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000560000000000000000000065
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000560000000000000000000655
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000650000600660066000666555
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000556006566556655666555555
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000565665555555555555555555
ddd00000000dd0000000000000000000ddd0000000000000cccccccccccc02200cccccccccccc9cc000000000000000000000000000000000000000000000000
0ffddd0000ddbd5000000000000000000ffddd0000000000cccccccc22000020002200ccc424499c000000000000000000000000000000000000000000000000
000fffdddddff556ddd00000000dd000000fffdddddddd000000200002000020000000002428244c000000000000000000000180000018000000000000000000
00000ffffdd000600ffddd0000ddbd5000000ffffddfddd0c000000000000000000000024244442400000000100000001000011a100011a00000000000000000
00000666ddd00000000fffdd0ddff55600000666fddfbdd0ccc0000000000000000000222442424400000000d1111100d1111100d11111000000000000000000
0000666f0000000000000ffffdd000600000666fdd056500cccccc000000000000000222424999cc000000000dd111100dd111100dd1111000000000f0000000
000066550000000000000666ddd000000000665500665000ccccccccc0000000000002222242499c00000000006111800056601000666510f00440000f054e00
000006655000000000000665500000000000066550ee6000ccccccccccc0000000200ccccc24444c0000000000561a0000f56a00006a59000f456e0000444000
0000000000000000ddd00000000dd000000000000000000000000000cccc000455cccccc00000000000000000000000000000000000000000000000000000000
dddd0000000dd0000ffddd0000ddbd50ddd0000000dbd56000000000cccc00445ccccccc0000000000000000011a000000000000000000000000600000060000
0fffddddddddbd50000fffdddddff5560ffddd000dddd5ee00000000cccc4445cccccccc00000000000000000011100000000000000000000000a500006a0000
0000ffffddfff55600000ffffdd00060000fffddddfff66e00000000ccc42255cccccccc00000000000000001001118000000000000000000000650000650000
0000666ddd00006000000666fddd000000000ffffd00000000000000ccc444555ccccccc0000000000000000d111111a1000000a000000000007650000655000
0006666f55500000000066600dd0000000000666ddd0000000000000cccc424555cccccc00000000000000000dd11100d1000081100001800006550000765000
00060000005000000006650006000000000066550000000000000000ccccc449556ccccc000000000000000000f56a000d111111d111111a0076500000076000
00066000000000000006500000000000000006655000000000000000ccccc499566ccccc00000000000000000000000000d111100dd111100065000000006500
0000000000000000ddd00000000dd000000000000000000000000000cccc000455cccccc00000000000000000000000000000000000000000000000000000000
dddd0000000dd0000ffddd0000ddbd50dddd00000000000000000000ccc0004555cccccc00000000000000000000000000000000000000000000000000000000
0fffddddddddbd50000fffdddddff5560fffddddddd0000000000000cc4244cc55cccccc00000000000000000000000000000000000000000000000000000000
0000ffffddfff55600000ffffdd00060000ffffffdddd00000000000cc442cc555555ccc00000000000000000000000000000000000000000000000000000000
00000666dd00006000000666fddd000000000666fddfdd0000000000cc444cc555556ccc00000000000000000000000000000000000000006700000000766600
00000566660000000000006650dd60000000666fdd0fbd0000000000ccc44cccccc66ccc00000000000000000000000000000000000000005667000007655a60
00000500060000000000000666560000000066550005d00000000000ccc249cccccccccc000000000000000000000000000000000000000005566a6066550000
00000550000000000000000065000000000006655065500000000000ccc499cccccccccc00000000000000000000000000000000000000000055550050000000
00000000000000000000000000000000000000000000000000000000cccc00005ccccccc00000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000cccc0004cccccccc00000000000000000000000000007000000000000000000000000000
00000000000000000000000000000000000000000000000000000000cccc5042cccccccc00000000000000000000000000007070000000000000000000000000
00000000000000000000000000000000000000000000000000000000ccc554444444cccc00000000000000000000000000707878e0f000000000000000000000
00000dddddd0000000000ddddd000000000000000000000000000000ccc555444249cccc0000000000000000000000000074ee7f878000000000000000000000
0000dfffddddd000000ddffffdddd000000000000000000000000000ccc555cccc99cccc0000000000000000000000000088f8e7e7f4f0000000000000000000
000df666f5dddd000ddfff66dddddd50000000000000000000000000cccc556ccccccccc00000000000000000000000000774fe877ee84000000000000000000
00ddf665655ffd00dff666655ddff556000000000000000000000000cccc566ccccccccc0000000000000000000000000844444f88787e7e0000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__gff__
0001110000000000000000020400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000080000000000080808080000000000000000000000000000040400000000000000000000000000000404000000000000000000000000008080000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0100000001010101010101010101010101010101004c00000101010101010101010101010101010000010101010101010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0100000000000000000000000000000101000000000000000000000000000000000000000000000000010000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
014c000000000000000000000000000101000000000c0c000000000000000000000000000000000000010000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010101000c000000000000000000000000000000000101000000000000000001010000000000000000010000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010101010100000000000000004d000b00000b4e000101004e00000000000001010000000000000101010000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0101010101404100000000000001010101010101010101010100000001010101010000000000000000010000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0101010101010100000000000000000101000000000000000000000000000001010101000000000000010101000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010000000000000000000b000000000101560000590000000000000000000001010101000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0100000000000000000101000000000101660000690000000000000000000001010101010101000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010000000000004d0c0101000000000101760000790000000000000000000001010000007900000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010000000000010101010100000000004c000000000000000000000000000001010000000000000000000000010101010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000001010000000000000046474849000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0c4e0b4e0c4e0b4e0c4e0b000000000c0b0c4e0c0b00005f005f000b4e0c4e0101007c7d0000000056575859000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0101010101010101010101000000010101010101010202020202020101010101010101010101010101010101010101010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0101010101010101010101000000010101010101010101010101010101010101010101010101010101010101010101010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
004c0000000000000000004e0c4c000000000000000000000000000000004c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01010000000000000b00000101010000000c4e00000000000000000000000101010000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0100000000000000010101010101010101010100000001010000000000000001010000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0100000000000000000000000000000101000000000001010000000000000001010000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010c4e4c0b000000000000000000000101000000004c01010000000000000001010000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0101010101010000000000000000000101000000010101010101000000000001010000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0101010101010101000000000000000101000000000000000000000000000001010000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010000000000000000000b4c0000000101000000000000000000000001010101010000007900000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0100000000000000000001010000000000000000000000000000000000000001010000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0100000000000000000001010000000000000000000000000000000000000001010000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010000000c004e0b0c4e01014e0c000b00004e0000000000000b0c4e0b0c4e01010000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0100000001010101010101010101010101010101000000000101010101010101010101010101010000010101010101010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
0101000000070010700207003070040700507006070070700c0700d0700e0700f0701007011070120701307018070190701a0701b0701c0701d0701e0701f0702407025070260702707028070290702a0702b070
011200003b7503b7503b750007003b7503b7503b750007000070036750387503b7503b75038750367503675031750317503175000700317503175031750007000070000700007000070000700007003675038750
011200003874038740387400000038740387403874000000000000000000000000000000000000000000000034750347503475000000347503475034750000000000000000000000000000000000000000000000
011200003472034720347200000034720347203472000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011200001c5102351025510235101c5102351025510235101c5102351025510235101c51023510255102351021510285102a5102851021510285102a5102851021510285102a5102851021510285102a51028510
011200003b7503b7503b750007003b7503b7503b750007003670036750387503b7503b75038750367500070034750347503475000700347503475034750007000070036750387503b7503b750387503675036750
011200003674036740367400000036740367403674000000000000000000000000000000000000000000000031750317503175000000317503175031750000000000000000000000000000000000000000000000
011200003372033720337200000033720337203372000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01120000235102a5102c5102a510235102a5102c5102a510235102a5102c5102a51020510285102a5102851020510285102a5102851020510285102a51028510235102a510215102a5102c510235102a51023510
01120000385553b5553b5053b5553b5053b5553b5053b55538555385053b5553b5053b5553b505385553855500505385553b5553b5053b5553b505385553b5553d5553b555005053b5553b555005050050500505
011200003b7103b7103b7103b7103b7103b7103b7103b7103b7103b7103b7103b7103971039710397103971038710387103871038710387103871038710387103871038710387103871038710387103871038710
011200003871038710387103871038710387103871038710387103871038710387103671036710367103671034710347103471034710347103471034710347103471034710347103471034710347103471034710
011200003b7103b7103b7103b7103b7103b7103b7103b7103b7103b7103b7103b7103971039710397103971038710387103871038710387103871038710387103b7103b7103b7103b71039710397103b7103b710
011200003871038710387103871038710387103871038710387103871038710387103671036710367103671034710347103471034710347103471034710347103471034710347103471034710347103471034710
0112000038555395553b5053b5553b5553b5053b5553b55538555385553b5553b5553b5053b5550050500505385553b5553b5053b5553b5053b5553b5053b5553d5553d5553b5553b5053b5553b5550050500505
01120000005053d555395550050539555005053955539555365553955500505395553d5553d5553b5553b555005050050500505005053b5553b555005053b5553d5553d5553b555005053b5553b5550050500505
01120000000053900539555395053955539555365553455500505365053950536555395550050539555005053d5553b5553b5053b5553b5053b5553b5053b5553d5553b5553b5053b5553b5553b5053b7053b004
011200003d5553b5553d5553b5553b5553b5253b555385553b5553b5553855500505005053b555385553b5553d5553b5553b555385553d5553855538555395553955539555005050050500505395553b5553b555
011200003b5553b55500505005050050500505005050050536505365553655536555365553655536555365553455534555385050050500505005050050538555385552f555315552f55538555385553855534555
011200003455534555000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
