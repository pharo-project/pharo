tests
testHasAbstractMethods
	self deny: Object hasAbstractMethods.
	self deny: Object class hasAbstractMethods.
		
	"Behavior has abstract methods, for example hasTraitComposition, basicLocalSelectors:"
	self assert: Behavior hasAbstractMethods.
	self deny: Behavior class hasAbstractMethods.
	
	
	self assert: DiskStore hasAbstractMethods.
	"DiskStore defines methods because its class side contains abstract methods"
	self assert: DiskStore class hasAbstractMethods.
	
	