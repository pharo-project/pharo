tests
testGlamourValue
	self assert: ([ :a | a + 2 ] glamourValue: 40) equals: 42.
	self assert: ([ :a | a + 2 ] glamourValue: 40 asGlamorousMultiValue) equals: 42.
	self assert: ([ 42 ] glamourValue: 40 asGlamorousMultiValue) equals: 42.
	self assert: ([ :a | a first + a last ] glamourValue: #(40 2)) equals: 42.
	self assert: ([ :a :b | a + b ] glamourValue: (GLMMultiValue withAll: #(40 2))) equals: 42.
	self assert: ([ :a | a + 2 ] glamourValue: (GLMMultiValue withAll: #(40 2))) equals: 42.
	self assert: ([ 42 ] glamourValue: (GLMMultiValue withAll: #(40 2))) equals: 42