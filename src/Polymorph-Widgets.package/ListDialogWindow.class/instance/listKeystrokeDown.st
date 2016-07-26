events
listKeystrokeDown
	listIndex = list size
		ifTrue: [ 
			self listIndex: 0.
			self giveFocusToSearch.
			^ true].
	^ false.