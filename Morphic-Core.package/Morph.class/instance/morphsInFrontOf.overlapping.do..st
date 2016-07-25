submorphs-accessing
morphsInFrontOf: someMorph overlapping: aRectangle do: aBlock
	"Evaluate aBlock with all top-level morphs in front of someMorph that overlap with the given rectangle. someMorph is either an immediate child of the receiver or nil (in which case all submorphs of the receiver are enumerated)."
	self submorphsDo:[:m|
		m == someMorph ifTrue:["Try getting out quickly"
			owner ifNil:[^self].
			^owner morphsInFrontOf: self overlapping: aRectangle do: aBlock].
		(m fullBoundsInWorld intersects: aRectangle)
			ifTrue:[aBlock value: m]].
	owner ifNil:[^self].
	^owner morphsInFrontOf: self overlapping: aRectangle do: aBlock.