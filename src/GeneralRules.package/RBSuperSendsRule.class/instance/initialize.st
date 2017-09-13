initialization
initialize
	super initialize.
	self 
		replace: 'super `@message: ``@args' 
		with: 'self `@message: ``@args'
		