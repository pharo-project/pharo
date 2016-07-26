comparing
= aRGGlobalDefinition
	"This method look for equality of the properties of the receiver"
	"Verifies the class and the name of the receiver"

	^self class = aRGGlobalDefinition class
		and:[ self name == aRGGlobalDefinition name ]