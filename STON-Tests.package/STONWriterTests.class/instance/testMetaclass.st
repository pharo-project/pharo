tests
testMetaclass
	self assert: (self serialize: Point class) = 'Metaclass[#Point]'