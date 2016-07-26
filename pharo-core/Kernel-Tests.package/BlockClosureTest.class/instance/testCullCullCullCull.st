tests - evaluating
testCullCullCullCull
	[  ]
		cull: 1
		cull: 2
		cull: 3
		cull: 4.
	[ :x |  ]
		cull: 1
		cull: 2
		cull: 3
		cull: 4.
	[ :x :y |  ]
		cull: 1
		cull: 2
		cull: 3
		cull: 4.
	[ :x :y :z |  ]
		cull: 1
		cull: 2
		cull: 3
		cull: 4.
	[ :x :y :z :a |  ]
		cull: 1
		cull: 2
		cull: 3
		cull: 4.
	self
		should: [ 
			[ :x :y :z :a :b |  ]
				cull: 1
				cull: 2
				cull: 3
				cull: 4 ]
		raise: Error.
	self
		assert:
			([ 0 ]
				cull: 1
				cull: 2
				cull: 3
				cull: 4) = 0.
	self
		assert:
			([ :x | x ]
				cull: 1
				cull: 2
				cull: 3
				cull: 4) = 1.
	self
		assert:
			([ :x :y | y ]
				cull: 1
				cull: 2
				cull: 3
				cull: 4) = 2.
	self
		assert:
			([ :x :y :z | z ]
				cull: 1
				cull: 2
				cull: 3
				cull: 4) = 3.
	self
		assert:
			([ :x :y :z :a | a ]
				cull: 1
				cull: 2
				cull: 3
				cull: 4) = 4