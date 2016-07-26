point functions
sortsBefore: otherPoint
	"Return true if the receiver sorts before the other point"
	^ y = otherPoint y
		ifTrue: [ x <= otherPoint x ]
		ifFalse: [ y <= otherPoint y ]