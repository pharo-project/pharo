initialization
initialize
	super initialize.

	self
		replace: 'self should: [ ``@object1 ]'
		with: 'self assert: ``@object1'