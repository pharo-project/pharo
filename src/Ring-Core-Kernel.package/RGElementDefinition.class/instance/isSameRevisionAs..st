testing
isSameRevisionAs: aRGElementDefinition
	"This method look for equality of the properties of the receiver"
	"Verifies the class and the parentName of the receiver"

	^self class = aRGElementDefinition class
		and:[ self parentName == aRGElementDefinition parentName ]