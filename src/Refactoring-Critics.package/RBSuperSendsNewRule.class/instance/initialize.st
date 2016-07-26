initialization
initialize
	super initialize.
	matcher := RBParseTreeSearcher new.
	matcher 
		matchesAnyOf: #(
			'super new initialize'
			'(super new: `@expr) initialize'
			'self new initialize'
			'(self new: `@expr) initialize' )
		do: [ :answer :node | true ].