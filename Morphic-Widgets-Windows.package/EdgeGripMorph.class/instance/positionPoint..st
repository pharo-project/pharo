as yet unclassified
positionPoint: aPoint
	"Reposition based on ptName."
	
	(#(top bottom) includes: self edgeName)
		ifTrue: [^self position: self left @ aPoint y].
	(#(left right) includes: self edgeName)
		ifTrue: [^self position: aPoint x @ self top].
	^self position: aPoint