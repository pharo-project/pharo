point functions
angleWith: aPoint
	"Answer the angle in radians between the vectors represented by the receiver and aPoint from the origin."

	| ar ap |
	ar := self angle.
	ap := aPoint angle.
	^ ap >= ar
		ifTrue: [ ap - ar ]
		ifFalse: [ Float pi * 2 - ar + ap ]