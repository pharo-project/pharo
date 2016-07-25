accessing
hasUnacceptedEdits: aBoolean
	"Set the hasUnacceptedEdits flag to the given value. "

	(self model respondsTo: #hasUnacceptedEdits:)
		ifTrue: [ self model hasUnacceptedEdits: aBoolean ]
		ifFalse: [ 
			super hasUnacceptedEdits: aBoolean.
			self changed ].
	aBoolean
		ifFalse: [ hasEditingConflicts := false ]