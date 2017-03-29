popup - listeners
addCloserListener

	self
		assert: [ self closer isNotNil ]
		description: [ 'Closer must not be nil' ].
	
	self closer popup: self.
	self activeHand addEventListener: self closer