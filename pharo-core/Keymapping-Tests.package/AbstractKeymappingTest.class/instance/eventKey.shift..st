utilities
eventKey: character shift: aBoolean
	^ self eventKey: character
		alt: false
		ctrl: false
		command: false
		shift: aBoolean