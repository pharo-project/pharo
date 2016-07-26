user interface
becomeModal
	self currentWorld
		ifNotNil: [self currentWorld modalWindow: self]