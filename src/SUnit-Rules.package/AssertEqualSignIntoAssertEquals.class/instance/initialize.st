initialization
initialize
	super initialize.
	self
		replace: 'self assert: ``@object1 = ``@object2'
		with: 'self assert: ``@object1 equals: ``@object2'