drawing
clipLayoutCells: aBool
	"Drawing/layout specific. If this property is set, clip the submorphs of the receiver by its cell bounds."
	aBool == false
		ifTrue:[self removeProperty: #clipLayoutCells]
		ifFalse:[self setProperty: #clipLayoutCells toValue: aBool].
	self changed.