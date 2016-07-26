geometry
scale: scaleFactor 
	| flex center ratio |
	ratio := self scaleFactor / scaleFactor.
	self borderWidth: ((self borderWidth / ratio) rounded max: 0).
	center := self referencePosition.
	flex := (MorphicTransform offset: center negated) withScale: ratio.
	self setVertices: (vertices collect: [:v | (flex transform: v) - flex offset]).
	super scale: scaleFactor.