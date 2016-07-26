initialization
unsubscribeListeners

	self activeHand removeEventListener: self.
	Morph announcer unsubscribe: self.