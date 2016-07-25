tests
testMetaclass
	self assert: (self materialize: 'Metaclass[#Point]') equals: Point class