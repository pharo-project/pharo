tests - evaluating
testCull
	[  ] cull: 1.
	[ :x |  ] cull: 1.
	self should: [ [ :x :y |  ] cull: 1 ] raise: Error.
	self should: [ [ :x :y :z |  ] cull: 1 ] raise: Error.
	self should: [ [ :x :y :z :a |  ] cull: 1 ] raise: Error.
	self should: [ [ :x :y :z :a :b |  ] cull: 1 ] raise: Error.
	self assert: ([ 0 ] cull: 1) = 0.
	self assert: ([ :x | x ] cull: 1) = 1