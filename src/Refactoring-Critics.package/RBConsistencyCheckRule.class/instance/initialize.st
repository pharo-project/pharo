initialization
initialize
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'`@object size == 0'
			'`@object size = 0'
			'`@object size > 0'
			'`@object size >= 1'
			'`@collection at: `@collection size' )
		do: [ :node :answer | node ]