accessing
hasBeenModified: aBool
	"Change the receiver to reflect the modification state"
	aBool ifTrue:[^self unhibernate].
	self shouldPreserveContents
		ifTrue:[self hibernate]
		ifFalse:[bits := nil]