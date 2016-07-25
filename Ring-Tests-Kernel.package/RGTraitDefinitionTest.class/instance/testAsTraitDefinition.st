testing
testAsTraitDefinition

	| newTrait |
	newTrait:= TBehavior asRingDefinition.
	self assert: (newTrait isRingObject).
	self assert: (newTrait isTrait).  
	self assert: (newTrait name == #TBehavior).  
	self assert: (newTrait category notNil).
	self assert: (newTrait superclassName notNil).
	
	
	self assert: (newTrait theMetaClass isRingObject).
	self assert: (newTrait theMetaClass isTrait).
	self assert: (newTrait theMetaClass traitCompositionSource = '{}').