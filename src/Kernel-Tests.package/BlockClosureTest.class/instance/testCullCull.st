tests - evaluating
testCullCull
	[  ] cull: 1 cull: 2.
	[ :x |  ] cull: 1 cull: 2.
	[ :x :y |  ] cull: 1 cull: 2.
	self should: [ [ :x :y :z |  ] cull: 1 cull: 2 ] raise: Error.
	self should: [ [ :x :y :z :a |  ] cull: 1 cull: 2 ] raise: Error.
	self should: [ [ :x :y :z :a :b |  ] cull: 1 cull: 2 ] raise: Error.
	self assert: ([ 0 ] cull: 1 cull: 2) = 0.
	self assert: ([ :x | x ] cull: 1 cull: 2) = 1.
	self assert: ([ :x :y | y ] cull: 1 cull: 2) = 2