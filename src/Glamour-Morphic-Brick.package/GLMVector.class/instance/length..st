initialization
length: aNumber

	self
		assert: [ aNumber >= 0 ]
		description: [ 'Length must be non-negative' ].

	length := aNumber