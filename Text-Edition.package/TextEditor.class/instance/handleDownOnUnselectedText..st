events
handleDownOnUnselectedText: event
	event shiftPressed
		ifTrue: [ ^ self mouseMove: event ].
	event yellowButtonPressed
		ifTrue: [ self yellowButtonDown: event ]
		ifFalse: [ 
			"no right/yellowbutton click just reposoition the cursor"
			self updateSelectionFromEvent: event ]