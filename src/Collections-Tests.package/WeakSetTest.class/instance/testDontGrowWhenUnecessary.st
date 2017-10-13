tests
testDontGrowWhenUnecessary
	| ws objs initialSize |
	objs := (2 to: 20) collect: [:i | 1 / i].
	
	ws := WeakSet new: 20.
	initialSize := ws array size.
	objs do: [:k | ws add: k].
	objs do: [:k | ws remove: k].
	self assert: ws array size = initialSize
		description: 'The WeakSet did not have to grow because enough room were preallocated'.
	
	objs do: [:k | ws add: k + 1].
	Smalltalk garbageCollect.
	objs do: [:k | ws add: k].
	self assert: ws array size = initialSize
		description: 'The WeakSet did not have to grow because slots of reclaimed objects were recycled'.