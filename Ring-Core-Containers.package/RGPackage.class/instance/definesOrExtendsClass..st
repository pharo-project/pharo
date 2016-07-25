testing
definesOrExtendsClass: aRGBehaviorDefinition
	"Returns true whether the class, aRGBehaviorDefinition, is one locally defined classes of the receiver or 
	if the receiver extends such class (that is defined in another package)"
	
	^ (self definesClass: aRGBehaviorDefinition) or:[ self extendsClass: aRGBehaviorDefinition ]