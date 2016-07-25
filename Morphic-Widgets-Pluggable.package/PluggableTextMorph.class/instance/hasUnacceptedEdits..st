styling
hasUnacceptedEdits: aBoolean
	"Set the hasUnacceptedEdits flag to the given value. "
	
	(model respondsTo: #hasUnacceptedEdits:)
		ifTrue: [ model hasUnacceptedEdits: aBoolean ]
		ifFalse: [ self basicHasUnacceptedEdits: aBoolean ]