testing
testAsPassive
	| newMethod |

	RGMethodDefinitionTest compile: 'foo	^ ''first version'''.
	newMethod := (RGMethodDefinitionTest >> #foo) asPassiveRingDefinition.
	self assert: newMethod isPassive.
	self assert: newMethod sourceCode = newMethod compiledMethod sourceCode.

	RGMethodDefinitionTest compile: 'foo	^ ''second version'''.
	self assert: newMethod sourceCode = 'foo	^ ''first version'''.		
	self assert: newMethod compiledMethod sourceCode = 'foo	^ ''second version'''.