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

-- draw debug messages to the screen
debuglog={}
function debug(...)
	local msg=""
	for m in all({...}) do
		msg=msg.." "..m
	end
 add(debuglog, msg)
 while #debuglog > 8 do
  del(debuglog, debuglog[1])
 end
end
function drawdebug()
	if isnight() then
 	color(8)
	else
		color(0)
	end
 cursor(4,20)
 for msg in all(debuglog) do
  print(msg)
 end
end

function drawstats()
	if showstats then
		rectfill(0,120, 54,127, 5)
		local m="#"..flr(stat(0))
		m=m.."\t%"..flr(stat(1)*100)
		m=m.."\ta:"..#world.actors
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

-- closest integer above x
function ceil(x)
 return -flr(-x)
end

-- returns 1 if x is positive, -1 if x is negative
function sign(x)
 return x/abs(x)
end

-- get sprite flags at map coord
function mfget(x,y,f)
 local s=mget(flr(x/8),flr(y/8))
 return fget(s,f)
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

function actor:setmiddle(args)
	if args.x then
		self.x=args.x-self.w*8/2
	end
	if args.y then
		self.y=args.y-self.h*8/2
	end
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

function actor:cansee(other)
 return not world:linecollides(self:middle(), other:middle())
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

-- a hook to perform actions when an actor gets hurt
-- calls despawn if health runs out
function actor:hurt(d)
	self.health-=d
	if self.health<=0 then
		self.health=0
		world:despawn(self)
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
	if pb>=self.y and pb<=self:hitbox().b and abs(d) < 64 then
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

	if self.walled then
		-- self.direction*=-1
		if self.grounded then
			self.vel.y-=self.jump
		end
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
 run={m=40},
	sprites={
		body=70,
		stand=87,
		walk={87,103,87,119},
 },
	w=4,
	h=2,
}

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
 btn={j=2,l=0,r=1,c=3,e=4,s=5},
 jd=dt*4, --0-1 in 1/4 seconds
 w=2,
 sprites={
  stand=64,
  walk={80,64,96,64},
  crouch=66,
		sleep=112,
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
		food=.6,
		water=.8,
		sleep=1,
	}
end

function player:munch(d)
	self.health=self.stats.health*10
	actor.munch(self, d)
	self.stats.health=self.health/10
end

function player:sprite()
	if self.sleeping then
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

function player:move()
	if self.sleeping then return end

 if btn(self.btn.l) then
  self.flipped=true
 elseif btn(self.btn.r) then
  self.flipped=false
 end
 if btn(self.btn.j) and self.grounded then
  self.j=min(self.j+self.jd,1)
 elseif self.j>0 then
  self.vel.y-=self.jump*(1+self.j)
  self.j=0
 end

 local run=self.run.m
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
	elseif btn(self.btn.l) and self.j<=0 then
  self.vel.x-=self.run.a*dt
  self.vel.x=max(self.vel.x,-run)
 elseif btn(self.btn.r) and self.j<=0 then
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
	if self.grounded and btn(self.btn.e) then
		local m=self:mouth()
		if #self.food>0 then
			self.eating=true
			self:eat()
		elseif world:actorcollides(self, sflags.cn) then
			self.eating=true
			self.stats.food+=dt*.05
			self.stats.water+=dt*.025
		elseif self:onwater() then
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
	actor.hurt(self, d)
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
	hd+=(1-min(self.stats.sleep*3, 1))*2
	self.stats.health-=d*hd
	for k,v in pairs(self.stats) do
		self.stats[k]=min(max(v,0),1)
	end
end

function player:drink()
	self.stats.water+=dt/20
end

function player:eat()
	local f=self.food[1]
	local a=f:munch(8*dt)/100
	self.stats.food+=a
	self.stats.water+=a/2
	f:setmiddle(self:mouth())
	f.y=self.y
	f.flipped=self.flipped
	if f.health<=0 then
		del(self.food, f)
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
 return {
  x=self.screens.x*self.tiles.w+self.tiles.d.x,
  y=self.screens.y*self.tiles.h+self.tiles.d.y,
  w=self.tiles.w,
  h=self.tiles.h,
 }
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
	if find(self.actors, actor)==nil then
 	add(self.actors, actor)
	else
		debug('double spawn '..actor.__name)
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
 self.spawns.fish={}

	-- find the critters on the map
 for x=b.x, b.w+b.x do
  for y=b.y, b.h+b.y do
   local s=mget(x,y)
   if fget(s,sflags.cs) then
				if majungasaurus:spriteset()[s] then
					if self.carrion[self:screenkey()] then
						self:spawn(majungasaurus(x*self.pixels.w, y*self.pixels.h))
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
		self.nextdanger=5+rnd(25)
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

function world:linecollides(p1, p2, flag)
	flag = flag or sflags.sm
	local dx=(p1.x-p2.x)/8
	local dy=(p1.y-p2.y)/8
	local s=dy/dx
	local a=0 z=0 i=0 lmget=false
	if dx>dy then
		a=flr(p1.x) z=flr(p2.x) i=sign(dx)
		lmget=function(x) return mget(x, flr(x*s)) end
	else
	 a=flr(p1.y) z=flr(p2.y) i=sign(dy)
		lmget=function(y) return mget(flr(y/s), y) end
	end

	for c=a,z,i do
		local spr=lmget(c)
		if fget(spr,flag) then
			return spr
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

function _update60()
	if gamestate==gs.gameover then return end
 if gamestate==gs.init then
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
			sleeptime=0
			wakingtime=.5
		end
		return
 end
	if wakingtime > 0 then
		wakingtime-=dt
	end

	protagonist:findfood(world.actors)
	protagonist:age(dt)

	if protagonist.stats.health<=0 then
		gamestate=gs.gameover
	elseif protagonist.sleeping then
		gamestate=gs.sleep
		world:startsleep()
	end
end

function drawsplash()
	rectfill(16,16,111,57,5)
	cursor(19,19)
	cprint("masiakasaurus knopfleri", 8)
	cprint(" mark knopfler's", 9)
	cprint(" vicious lizard", 9)
	cprint(" z to eat", 6)
	cprint(" x to run", 6)
	cprint(" x or z to start", 7)
end

function drawgameover()
	rectfill(0,0, 127,127, 0)
	cursor(16,16)
	cprint("game over", 8)
end

function drawsleep(time)
	local c=5
	if (isnight()) c=6
	local p=protagonist:middle()
	world:print("ZZZ", p.x-4, p.y-8, 5)
end

function _drawsleep(time)
	local o=world:offset()
	local pb=protagonist:hitbox()
	pb=box(
		pb.x-pb.w-o.x,
		pb.y-pb.h-o.y,
		pb.w*3, pb.h*3
	)
	local pm={
		x=pb.x+pb.w/2,
		y=pb.y+pb.h/2,
	}
	local c={5,6}
	for x=0,127 do
		for y=16,127 do
			local r=rnd()
			if pb:contains({x=x,y=y}) then
				local dx=(x-pm.x)/(pb.w/2)
				local dy=(y-pm.y)/(pb.h/2)
				if dx*dx+dy*dy < 1 then
					r=time
				end
			end
			if r<time then
				r=r*1/time
				pset(x,y,c[flr(r*#c)+1])
			end
		end
	end
end

function _draw()
	if gamestate==gs.gameover then
		drawgameover()
		return
	end
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
 drawdebug()
	drawstats()
end
__gfx__
70000007333333331111111100000000000000000000000000000000000000000000000000000000000000000bb0bbb00333033055ee2ee55555555555555555
07000070b3bb333bcdc7cc7c0000000000000000000000000000000000000000000000000000000000000000b00b0000000030035e70e07e5ec55ec55ec55555
00700700bb3bb3bb1111ccc10000000000000000000000000000000000000000000000000000000000000000000490bb330440005700000ee7eee7eee7eeee55
00077000fbbfbbb37ccc1d1c000000000000000000000000000000000000000000000000000000000000000000099900004440005e00000000e0000000e000e5
00077000fbfbfbbfc1c11ccc0000000000000000000000000000000000000000000000000000000000000000b00940000004f0035700000000000e00000000e5
00700700bfbfbfffbcbbcbcb00000000000000000000000000000000000000000000000000000000000000000b0990000004403052e000eeee7eee7eee7eee55
07000070ff4ffbfbbfbfbbfb000000000000000000000000000000000000000000000000000000000000000000999000000f4400557e0e7555ec55ec55ec5555
70000007f4ff4f4ff4fff4ff000000000000000000000000000000000000000000000000000000000000000000049000000440005552e2555555555555555555
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
00000666ddd00000000fffdddddff55600000666fddfbdd0ccc0000000000000000000222442424400000000d1111100d1111100d11111000000000000000000
0000666f0000000000000ffffdd000600000666fdd056500cccccc000000000000000222424999cc000000000dd111100dd111100dd1111000000000f0000000
000066550000000000000666ddd000000000665500665000ccccccccc0000000000002222242499c00000000006111800056601000666510f00440000f054e00
000006655000000000000665500000000000066550ee6000ccccccccccc0000000200ccccc24444c0000000000561a0000f56a00006a59000f456e0000444000
0000000000000000ddd00000000dd000000000000000000000000000cccc000455cccccc00000000000000000000000000000000000000000000000000000000
dddd0000000dd0000ffddd0000ddbd50ddd0000000dbd56000000000cccc00445ccccccc0000000000000000011a000000000000000000000000600000060000
0fffddddddddbd50000fffdddddff5560ffddd000dddd5ee00000000cccc4445cccccccc00000000000000000011100000000000000000000000a500006a0000
0000ffffddfff55600000ffffdd00060000fffddddfff66e00000000ccc42255cccccccc00000000000000001001118000000000000000000000650000650000
0000666ddd00006000000666fddd000000000ffffd00000000000000ccc444555ccccccc0000000000000000d111111a1000000a000000000007650000655000
0006666f55500000000066600dd0000000000666ddd0000000000000ccc4424555cccccc00000000000000000dd11100d1000081100001800006550000765000
00060000005000000006650006000000000066550000000000000000cccc2449556ccccc000000000000000000f56a000d111111d111111a0076500000076000
00066000000000000006500000000000000006655000000000000000cccc4499566ccccc00000000000000000000000000d111100dd111100065000000006500
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
00000dddddd000000000000000000000000000000000000000000000ccc555444249cccc0000000000000000000000000074ee7f878000000000000000000000
0000dfffddddd0000000000000000000000000000000000000000000ccc555cccc99cccc0000000000000000000000000088f8e7e7f4f0000000000000000000
000df666f5dddd000000000000000000000000000000000000000000cccc556ccccccccc00000000000000000000000000774fe877ee84000000000000000000
00ddf665655ffd000000000000000000000000000000000000000000cccc566ccccccccc0000000000000000000000000844444f88787e7e0000000000000000
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
010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011200003b0503b0503b050005003b0503b0503b050000000000036050380503b0503b05038050360503605031050310503105000000310503105031050000000000000000000000000000000000003605038050
011200003874038740387400000038740387403874000000000000000000000000000000000000000000000034750347503475000000347503475034750000000000000000000000000000000000000000000000
011200003472034720347200000034720347203472000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011200001c5102351025510235101c5102351025510235101c5102351025510235101c51023510255102351021510285102a5102851021510285102a5102851021510285102a5102851021510285102a51028510
011200003b0503b0503b050000003b0503b0503b050000003600036050380503b0503b05038050360500000034050340503405000000340503405034050000000000036050380503b0503b050380503605036050
011200003674036740367400000036740367403674000000000000000000000000000000000000000000000031750317503175000000317503175031750000000000000000000000000000000000000000000000
011200003372033720337200000033720337203372000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01120000235102a5102c5102a510235102a5102c5102a510235102a5102c5102a51020510285102a5102851020510285102a5102851020510285102a51028510235102a510215102a5102c510235102a51023510
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
02 05060708
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
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
