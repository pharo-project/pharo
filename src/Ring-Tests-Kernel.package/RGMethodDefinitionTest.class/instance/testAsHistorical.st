testing
testAsHistorical
	| firstVersion secondVersion |

	RGMethodDefinitionTest compile: 'foo	^ ''first version'''.
	firstVersion := (RGMethodDefinitionTest >> #foo) asHistoricalRingDefinition.
	self assert: firstVersion isHistorical.
	self assert: firstVersion sourceCode = firstVersion compiledMethod sourceCode.
	self assert: firstVersion stamp = firstVersion compiledMethod timeStamp.

	RGMethodDefinitionTest compile: 'foo	^ ''second version'''.
	secondVersion := (RGMethodDefinitionTest >> #foo) asHistoricalRingDefinition.
	self assert: secondVersion isHistorical.
	self assert: secondVersion sourceCode = secondVersion compiledMethod sourceCode.
	self assert: secondVersion stamp = secondVersion compiledMethod timeStamp.

	self deny: firstVersion sourcePointer = secondVersion sourcePointer.
	self deny: firstVersion sourceCode = secondVersion sourceCode.