tests
testAddIncludesSizeReclaim
	| ws o2 o3 |
	o2 := 1/2.
	o3 := '123' copy.
	ws := WeakSet new.
	ws add: o2.
	ws add: o3.
	self assert: ws size = 2.
	self assert: (ws includes: o2).
	self assert: (ws includes: o3).
	
	"inclusion test does use equality, not identity"
	self assert: (ws includes: o3 copy) description: 'WeakSet are not WeakIdentitySet'.
	
	"only one copy is added"
	ws add: o3 copy.
	self assert: ws size = 2.
	
	"reclame objects so that slots of ws are nilled out"
	o2 := o3 := nil.
	Smalltalk garbageCollect.
	self deny: (ws includes: 1/2).
	self deny: (ws includes: '123' copy).
	
	"fast #size is not updated automatically by dead object reclamation
	But there is a slowSize trying to tell the truth"
	self assert: ws slowSize = 0.