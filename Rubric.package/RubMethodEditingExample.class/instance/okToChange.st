updating
okToChange
	self textModel canDiscardEdits ifTrue: [ ^ true].
	self promptForCancel ifTrue: [ self textModel clearUserEdits  ].
	^ self textModel canDiscardEdits