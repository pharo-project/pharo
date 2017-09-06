initialization
initialize
	super initialize.
	self  matchesAny: #(
		'`@object size == 0'
		'`@object size = 0'
		'`@object size > 0'
		'`@object size >= 1'
		'`@collection at: `@collection size' )
		