initialization
initialize
	super initialize.
	self matchesAny: #(
			'`#floatLiteral = `@expr'
			'`#floatLiteral ~= `@expr'
			'`@expr = `#floatLiteral'
			'`@expr ~= `#floatLiteral' )