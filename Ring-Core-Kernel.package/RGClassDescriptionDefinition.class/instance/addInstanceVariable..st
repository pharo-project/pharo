instance variables
addInstanceVariable: aRGVariableDefinition
	"aRGVariableDefinition is a instance variable or class instance variable"

	self addVariable: (aRGVariableDefinition parent: self)
		in: instanceVariables