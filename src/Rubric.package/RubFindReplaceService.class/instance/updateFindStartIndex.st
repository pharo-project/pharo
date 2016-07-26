updating
updateFindStartIndex
	| si |
	self textArea 
		ifNil: [ ^self ]
		ifNotNil: [ :aTextArea |
			aTextArea changed.
			self changed: #replaceEnabled.
			si := aTextArea selectionInterval.
			self findStartIndex: (self searchBackwards ifTrue: [si first - 1] ifFalse: [si last + 1]) ]