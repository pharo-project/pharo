morphic geometry
invalidRect: damageRect from: aMorph
	"Translate damage reports from submorphs by the scrollOffset."
	aMorph == self
		ifTrue: [super invalidRect: damageRect from: self]
		ifFalse: [ | globalDamageRect myDamageRect |
			globalDamageRect := self transform localBoundsToGlobal: damageRect.
			myDamageRect := globalDamageRect intersect: bounds ifNone: [ ^ self ].
			super invalidRect: (myDamageRect expandBy: 1) from: self]
