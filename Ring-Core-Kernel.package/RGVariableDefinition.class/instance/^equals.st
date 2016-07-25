comparing
= aRGVariableDefinition
	"This variable look for equality of the key properties of the receiver"

	^(super = aRGVariableDefinition)
		and:[ self name == aRGVariableDefinition name ]