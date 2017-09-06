initialization
initialize
	super initialize.
	self matchesAny: #(
			'``@object = `#literal or: [``@expression]' 
			'``@object == `#literal or: [``@expression]' 
			'`#literal = ``@object or: [``@expression]' 
			'`#literal == ``@object or: [``@expression]' 
			'``@expression | (``@object = `#literal)' 
			'``@expression | (``@object == `#literal)' 
			'``@expression | (`#literal = ``@object)' 
			'``@expression | (`#literal == ``@object)')