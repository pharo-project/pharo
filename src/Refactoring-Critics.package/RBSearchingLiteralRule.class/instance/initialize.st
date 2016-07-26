initialization
initialize
	super initialize.
	self matcher
		matchesAnyOf: #(
			'``@object = `#literal or: [``@expression]' 
			'``@object == `#literal or: [``@expression]' 
			'`#literal = ``@object or: [``@expression]' 
			'`#literal == ``@object or: [``@expression]' 
			'``@expression | (``@object = `#literal)' 
			'``@expression | (``@object == `#literal)' 
			'``@expression | (`#literal = ``@object)' 
			'``@expression | (`#literal == ``@object)')
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					(self isSearchingLiteralExpression: node)
						ifTrue: [ node ]
						ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]