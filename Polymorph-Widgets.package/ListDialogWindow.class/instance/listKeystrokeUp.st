events
listKeystrokeUp
	listIndex = 1 
		ifTrue: [ 
			self listIndex: 0.
			self giveFocusToSearch.
			^ true].
	^ false.