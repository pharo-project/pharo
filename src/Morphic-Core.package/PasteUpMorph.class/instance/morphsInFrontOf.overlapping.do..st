submorphs-accessing
morphsInFrontOf: someMorph overlapping: aRectangle do: aBlock
	"Include hands if the receiver is the World"
	self handsDo:[:m|
		m == someMorph ifTrue:["Try getting out quickly"
			owner ifNil:[^self].
			^owner morphsInFrontOf: self overlapping: aRectangle do: aBlock].
		"The hand only overlaps if it's not the hardware cursor"
		m needsToBeDrawn ifTrue:[
			(m fullBoundsInWorld intersects: aRectangle)
				ifTrue:[aBlock value: m]]].
	^super morphsInFrontOf: someMorph overlapping: aRectangle do: aBlock