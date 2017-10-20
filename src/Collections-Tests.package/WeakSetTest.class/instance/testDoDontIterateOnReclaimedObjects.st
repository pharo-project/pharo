tests
testDoDontIterateOnReclaimedObjects

	| ws o1 o2 size |
	ws := WeakSet new.
	2 to: 20 do: [:i | ws add: 1 / i].
	
	o1 := 3.0.
	o2 := 4.0.
	ws add: o1; add: o2.
	
	"Reclaim memory"
	Smalltalk garbageCollect.
	
	size := 0.
	ws do: [:each |
		size := size + 1.
		self deny: each isNil].
	
	self assert: size = 2