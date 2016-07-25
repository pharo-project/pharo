initialization
initialize
	super initialize.

	self rewriteRule
		replace: 'self should: [ ``@object1 ]'
		with: 'self assert: ``@object1'