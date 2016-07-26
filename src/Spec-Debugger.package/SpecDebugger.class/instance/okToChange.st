api
okToChange
	self code canDiscardEdits
		ifTrue: [ ^ true ].
	self code changed: #wantToChange.
	^ self code canDiscardEdits