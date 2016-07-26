testing
isSpecial
	"I am just a backward compatible ivar slot. Note: my subclasses are special"
	^(self class = InstanceVariableSlot) not