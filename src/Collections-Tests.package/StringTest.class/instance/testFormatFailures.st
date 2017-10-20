tests - formatting
testFormatFailures

	self
		should: [ 'This is {foo} !' format: Dictionary new ]
		raise: KeyNotFound.
		
	self
		should: [ 'This is {1} !' format: Array new ]
		raise: SubscriptOutOfBounds.
