*Komitter-UI
koSourceText

	^ self isDirty
		ifFalse: [ 'No changes' ]
		ifTrue: [ self definition koSourceText ]