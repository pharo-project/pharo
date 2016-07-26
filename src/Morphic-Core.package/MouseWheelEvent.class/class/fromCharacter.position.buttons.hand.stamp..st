instance creation
fromCharacter: aCharacter position: aPoint buttons: evtButtons hand: evtHand stamp: stamp

	^ self new
		setType: #mouseWheel
		position: aPoint
		direction: aCharacter
		buttons: evtButtons
		hand: evtHand
		stamp: stamp