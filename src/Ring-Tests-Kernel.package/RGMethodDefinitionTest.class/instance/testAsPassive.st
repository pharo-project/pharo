testing
testAsPassive
	| newMethod |

	RGMethodDefinitionTest compileSilently: 'foo	^ ''first version''' classified: 'test'.
	newMethod := (RGMethodDefinitionTest >> #foo) asPassiveRingDefinition.
	self assert: newMethod isPassive.
	self assert: newMethod sourceCode = newMethod compiledMethod sourceCode.

	RGMethodDefinitionTest compileSilently: 'foo	^ ''second version''' classified: 'test'.
	self assert: newMethod sourceCode = 'foo	^ ''first version'''.		
	self assert: newMethod compiledMethod sourceCode = 'foo	^ ''second version'''.