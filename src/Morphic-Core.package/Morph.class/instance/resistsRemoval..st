accessing
resistsRemoval: aBoolean
	"Set the receiver's resistsRemoval property as indicated"

	aBoolean
		ifTrue: [ self setProperty: #resistsRemoval toValue: true ]
		ifFalse: [ self removeProperty: #resistsRemoval ]