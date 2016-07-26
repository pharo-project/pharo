*Komitter-UI
koDestinationText

	^ self isDirty
		ifTrue: [ self definition koDestinationText ]
		ifFalse: [ '' ]