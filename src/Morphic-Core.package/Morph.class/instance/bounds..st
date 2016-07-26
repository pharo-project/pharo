geometry
bounds: newBounds
	| oldExtent newExtent |
	oldExtent := self extent.
	newExtent := newBounds extent.
	(oldExtent dotProduct: oldExtent) <= (newExtent dotProduct: newExtent) ifTrue:[
		"We're growing. First move then resize."
		self position: newBounds topLeft; extent: newExtent.
	] ifFalse:[
		"We're shrinking. First resize then move."
		self extent: newExtent; position: newBounds topLeft.
	].