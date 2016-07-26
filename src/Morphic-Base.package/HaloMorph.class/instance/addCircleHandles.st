private
addCircleHandles
	| box |
	target isWorldMorph ifTrue: [^ self addHandlesForWorldHalos].

	self removeAllMorphs.  "remove old handles, if any"
	self bounds: (self worldBoundsForMorph: target renderedMorph).  "update my size"
	box := self basicBox.

	target addHandlesTo: self box: box.

	self addName.
	growingOrRotating := false.
	self layoutChanged.
	self changed.
