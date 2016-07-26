initialization
initialize
	super initialize.
	self rewriteRule
		replace: 'self assert: ``@object1 = ``@object2'
		with: 'self assert: ``@object1 equals: ``@object2'