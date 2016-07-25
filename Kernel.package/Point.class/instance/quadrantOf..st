point functions
quadrantOf: otherPoint
	"Return 1..4 indicating relative direction to otherPoint.
	1 is downRight, 2=downLeft, 3=upLeft, 4=upRight"
	^ x <= otherPoint x
		ifTrue: [ y < otherPoint y ifTrue: [1] ifFalse: [4] ]
		ifFalse: [ y <= otherPoint y ifTrue: [2] ifFalse: [3] ]
