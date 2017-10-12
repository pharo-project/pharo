tests
testGrowWhenNecessary
	| ws objs initialSize |
	objs := (2 to: 20) collect: [:i | 1 / i].
	ws := WeakSet new: 5.
	initialSize := ws array size.
	1 to: objs size do: [:k | ws add: (objs at: k)].
	self assert: ws array size > initialSize
		description: 'The WeakSet grown because not enough room were preallocated'.