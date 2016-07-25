instance creation
class: aRGBehaviorDefinition selector: aString
	"Create a ring method definition from a ring class and a selector"

	^(self class: aRGBehaviorDefinition)
		selector: aString asSymbol;
		yourself