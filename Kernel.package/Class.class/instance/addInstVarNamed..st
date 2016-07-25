instance variables
addInstVarNamed: aString
	"Add the argument, aString, as one of the receiver's instance variables."
	aString substrings do: [ :each|
		self addSlot: (InstanceVariableSlot named: each asSymbol)]
