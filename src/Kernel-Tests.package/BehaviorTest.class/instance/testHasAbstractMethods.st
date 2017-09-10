tests
testHasAbstractMethods
	self deny: Object hasAbstractMethods.
	self deny: Object class hasAbstractMethods.
		
	"Behavior has abstract methods, for example hasTraitComposition, basicLocalSelectors:"
	self assert: Behavior hasAbstractMethods.
	self deny: Behavior class hasAbstractMethods.
	
	
	self assert: ObjectLayout hasAbstractMethods.
	"ObjectLayout defines methods because its class side contains abstract methods"
	self assert: ObjectLayout class hasAbstractMethods.
	
	