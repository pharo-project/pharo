testing
containsPoint: aPoint in: rect
	"Answer whther the bounds contain the given (local coordinate) point."

	|r|
	r := self bounds ifNil: [^ false ].
	r := self fillWidth
		ifTrue: [rect left @ r top corner: (rect right @ r bottom)]
		ifFalse: [r]. 
	^r containsPoint: aPoint