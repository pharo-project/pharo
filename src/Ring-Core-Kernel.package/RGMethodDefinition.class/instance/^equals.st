comparing
= aRGMethodDefinition
	"This method look for equality of the key properties of the receiver"


	^(super = aRGMethodDefinition)
		and: [ self selector == aRGMethodDefinition selector]