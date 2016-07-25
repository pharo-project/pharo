testing
isSameRevisionAs: aRGBehaviorDefinition
	"This method look for equality of the properties of the receiver"

	^self class = aRGBehaviorDefinition class
		and:[ self name == aRGBehaviorDefinition name ]