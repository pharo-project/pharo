instance variables
addInstVarNamed: aString
	"Add the argument, aString, as one of the receiver's instance variables."

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	aString substrings do: [ :each|
		self addSlot: (InstanceVariableSlot named: each asSymbol)]
