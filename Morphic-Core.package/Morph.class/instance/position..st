geometry
position: aPoint 
	"Change the position of this morph and and all of its
	submorphs. "
	| delta box |
	delta := aPoint asNonFractionalPoint - bounds topLeft.
	(delta x = 0
			and: [delta y = 0])
		ifTrue: [^ self].
	"Null change"
	box := self fullBounds.
	(delta dotProduct: delta)
			> 100
		ifTrue: ["e.g., more than 10 pixels moved"
			self invalidRect: box.
			self
				invalidRect: (box translateBy: delta)]
		ifFalse: [self
				invalidRect: (box
						merge: (box translateBy: delta))].
	self privateFullMoveBy: delta.
	owner
		ifNotNil: [owner layoutChanged]