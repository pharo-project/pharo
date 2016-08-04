testing
testAsActive
	| newMethod |

	RGMethodDefinitionTest compileSilently: 'foo	^ ''first version''' classified: 'test'.
	newMethod := (RGMethodDefinitionTest >> #foo) asActiveRingDefinition.
	self assert: newMethod isActive.	
	self assert: newMethod sourcePointer notNil.
	self assert: newMethod sourceCode = newMethod compiledMethod sourceCode.

	RGMethodDefinitionTest compileSilently: 'foo	^ ''second version''' classified: 'test'.
	self assert: newMethod sourceCode = newMethod compiledMethod sourceCode.
	
	newMethod := (Trait >> #asRingDefinition) asActiveRingDefinition.
	newMethod fromActiveToPassive.
	self assert: newMethod isPassive.
	self assert: newMethod sourceCode = (Trait >> #asRingDefinition) sourceCode.
	newMethod sourceCode: 'asRingDefinition   ^true'.
	self assert: newMethod sourceCode ~= (Trait >> #asRingDefinition) sourceCode.
	
	newMethod := (Trait >> #asRingDefinition) asActiveRingDefinition.
	newMethod fromActiveToHistorical.
	self assert: newMethod isHistorical.
	self assert: newMethod sourcePointer notNil.