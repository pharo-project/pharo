model protocol
basicHasUnacceptedEdits: aBoolean
	self autoAccept
		ifTrue: [ ^ self ].
	aBoolean = hasUnacceptedEdits 
		ifFalse: [ hasUnacceptedEdits := aBoolean.
			self changed].
	aBoolean 
		ifFalse: [hasEditingConflicts := false]