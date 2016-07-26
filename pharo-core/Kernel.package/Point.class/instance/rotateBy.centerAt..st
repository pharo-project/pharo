point functions
rotateBy: direction centerAt: c 
	"Answer a Point which is rotated according to direction, about the point c.
	Direction must be one of #right (CW), #left (CCW) or #pi (180 degrees)."
	| offset |
	offset := self - c.
	direction == #right ifTrue: [ ^ offset y negated @ offset x + c ].
	direction == #left ifTrue: [ ^ offset y @ offset x negated + c ].
	direction == #pi ifTrue: [ ^ c - offset ].
	self error: 'unrecognizable direction'