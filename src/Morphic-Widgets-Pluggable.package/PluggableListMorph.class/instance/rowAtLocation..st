accessing
rowAtLocation: aPoint
	"Return the row at the given point or 0 if outside"
	| pointInListMorphCoords |
	pointInListMorphCoords := (self scroller transformFrom: self) transform: aPoint.
	^self listMorph rowAtLocation: pointInListMorphCoords.