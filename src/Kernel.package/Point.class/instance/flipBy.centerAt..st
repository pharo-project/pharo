point functions
flipBy: direction centerAt: c
	"Answer a Point which is flipped according to the direction about the point c. Direction must be #vertical or #horizontal."
	direction == #vertical ifTrue: [ ^ x @ (c y * 2 - y) ].
	direction == #horizontal ifTrue:  [^ (c x * 2 - x) @ y ].
	self error: 'unrecognizable direction'