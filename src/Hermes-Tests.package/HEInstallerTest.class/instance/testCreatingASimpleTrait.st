tests
testCreatingASimpleTrait
	| exportedTrait |
	
	exportedTrait := HETrait for: THEOneTestTrait. 
	self changeNameOf: exportedTrait to: #THEOneTestTraitNew.
	
	aTrait := builder buildTrait: exportedTrait.	
	builder installMethods: exportedTrait into: aTrait.

	self assert: aTrait traitComposition isEmpty.
	self assert: aTrait theMetaClass traitComposition isEmpty.
	
	self assert: aTrait methods size equals: THEOneTestTrait methods size.
	self assert: aTrait classTrait methods size equals: THEOneTestTrait classTrait methods size.	
	
	aTrait >> #oneMessage.
	aTrait >> #oneMessageRequired