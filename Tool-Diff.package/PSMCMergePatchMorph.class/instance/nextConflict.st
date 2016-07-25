as yet unclassified
nextConflict
	"Answer the next (unresolved) conflict or, nil if none."

	|coll current index|
	current := self selectedChangeWrapper.
	index := current isNil
		ifTrue: [0]
		ifFalse: [self model indexOf: current item].
	coll := (self model
			copyFrom: index + 1
			to: self model size),
		(self model copyFrom: 1 to: index).
	^coll detect: [:item | item isConflict and: [item isResolved not]] ifNone: [nil]