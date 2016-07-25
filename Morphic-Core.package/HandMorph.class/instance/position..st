geometry
position: aPoint
	"Overridden to align submorph origins to the grid if gridding is on."
	| adjustedPosition delta box |
	adjustedPosition := aPoint.
	temporaryCursor ifNotNil: [adjustedPosition := adjustedPosition + temporaryCursorOffset].

	"Copied from Morph to avoid owner layoutChanged"
	"Change the position of this morph and and all of its submorphs."
	delta := adjustedPosition - bounds topLeft.
	(delta x = 0 and: [delta y = 0]) ifTrue: [^ self].  "Null change"
	box := self fullBounds.
	(delta dotProduct: delta) > 100 ifTrue:[
		"e.g., more than 10 pixels moved"
		self invalidRect: box.
		self invalidRect: (box translateBy: delta).
	] ifFalse:[
		self invalidRect: (box merge: (box translateBy: delta)).
	].
	self privateFullMoveBy: delta.
