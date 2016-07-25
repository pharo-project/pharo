testing
extendsClass: aRGBehaviorDefinition
	"Returns true if the receiver extends aRGBehaviorDefinition (that is defined in another package)"
	
	^ self extendedClasses includes: aRGBehaviorDefinition