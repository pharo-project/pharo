initialization
initialize
	super initialize.
	self matchesAny: #(
			'(`@a and: `@b) and: `@c'
			'(`@a or: `@b) or: `@c' )