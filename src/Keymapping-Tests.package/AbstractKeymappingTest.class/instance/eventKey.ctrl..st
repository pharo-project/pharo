utilities
eventKey: character ctrl: aBoolean
	^ self eventKey: character
		alt: false
		ctrl: aBoolean
		command: false
		shift: false