slots
usesSpecialSlot
	"return true if we define something else than InstanceVariableSlots"
	^self slots anySatisfy: [ :each | each isSpecial ]