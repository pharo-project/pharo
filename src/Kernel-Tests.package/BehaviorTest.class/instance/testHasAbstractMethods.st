tests
testHasAbstractMethods
	self deny: Object hasAbstractMethods.
	self deny: Object class hasAbstractMethods.
		
	"Behavior has abstract methods, for example hasTraitComposition, basicLocalSelectors:"
	self assert: Behavior hasAbstractMethods.
	self deny: Behavior class hasAbstractMethods.
	