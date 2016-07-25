pixel access
colorAt: aPoint
	"Answer the color in the world at the given point."
	
	^self isInWorld
		ifTrue: [(Display colorAt: aPoint) asNontranslucentColor ]
		ifFalse: [Color black]