drop shadows
shadowPoint: newPoint
	self changed.
	self shadowOffset: newPoint - self center // 5.
	fullBounds ifNotNil:[fullBounds := self privateFullBounds].
	self changed.