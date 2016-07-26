accessing
aaLevel: newLevel
	"Only allow changes to aaLevel if we're working on >= 8 bit forms"
	form depth >= 8 ifFalse:[^self].
	aaLevel = newLevel ifTrue:[^self].
	self flush.	"In case there are pending primitives in the engine"
	aaLevel := newLevel.
	engine ifNotNil:[engine aaLevel: aaLevel].