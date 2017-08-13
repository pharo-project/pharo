tests
testCreatingATraitUsingOthers
	| exportedTrait |
	
	exportedTrait := HETrait for: THEATraitUsingOthers. 
	self changeNameOf: exportedTrait to: #THEATraitUsingOthersNew. 
	
	aTrait := builder buildTrait: exportedTrait.	
	builder installMethods: exportedTrait into: aTrait.

	self deny: aTrait traitComposition isEmpty.
	self deny: aTrait theMetaClass traitComposition isEmpty.
	
	self assert: aTrait methods size equals: THEATraitUsingOthers methods size.
	self assert: aTrait classTrait methods size equals: THEATraitUsingOthers classTrait methods size.	
		
	self assert: aTrait traitComposition printString equals: THEATraitUsingOthers traitComposition printString.

	self assert: aTrait classTrait traitComposition printString equals: THEATraitUsingOthers classTrait traitComposition printString.
	
	aTrait >> #oneMessage.
	aTrait >> #oneMessageRequired