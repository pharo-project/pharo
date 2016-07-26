tests
testAsGlamorousMultiValue
	self assert: (GLMMultiValue with: #(1 2 3)) equals: #(1 2 3) asGlamorousMultiValue.
	self
		assert: 1 asGlamorousMultiValue , 2 asGlamorousMultiValue , 3 asGlamorousMultiValue
		equals: (GLMMultiValue withAll: #(1 2 3))