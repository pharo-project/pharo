initialization
initialize
	super initialize.
	self matchesAny: #(
			'super new initialize'
			'(super new: `@expr) initialize'
			'self new initialize'
			'(self new: `@expr) initialize' )