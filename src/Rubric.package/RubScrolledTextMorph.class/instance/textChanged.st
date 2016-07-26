event handling
textChanged
	self manageLayout.
	self autoAccept
		ifTrue: [ self acceptContents ]
		ifFalse: [ self hasUnacceptedEdits: true ].
	self rulers do: [ :r | r textChanged ]