edits
okToChange
	self canDiscardEdits
		ifTrue: [ ^ true ].
	^ self promptForCancel 