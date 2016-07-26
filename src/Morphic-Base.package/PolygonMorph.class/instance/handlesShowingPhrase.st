menu
handlesShowingPhrase
	^ (self showingHandles
		ifTrue: ['hide handles']
		ifFalse: ['show handles']) translated