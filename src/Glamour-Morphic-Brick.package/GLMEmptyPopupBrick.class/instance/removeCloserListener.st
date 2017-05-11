popup - listeners
removeCloserListener

	self
		assert: [ self closer isNotNil ]
		description: [ 'Closer must not be nil' ].
	
	self activeHand removeEventListener: self closer.
	self closer popup: nil