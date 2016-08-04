testing
testAsHistorical
	| firstVersion secondVersion |

	RGMethodDefinitionTest compileSilently: 'foo	^ ''first version''' classified: 'test'.
	firstVersion := (RGMethodDefinitionTest >> #foo) asHistoricalRingDefinition.
	self assert: firstVersion isHistorical.
	self assert: firstVersion sourceCode = firstVersion compiledMethod sourceCode.
	self assert: firstVersion stamp = firstVersion compiledMethod timeStamp.

	RGMethodDefinitionTest compileSilently: 'foo	^ ''second version''' classified: 'test'.
	secondVersion := (RGMethodDefinitionTest >> #foo) asHistoricalRingDefinition.
	self assert: secondVersion isHistorical.
	self assert: secondVersion sourceCode = secondVersion compiledMethod sourceCode.
	self assert: secondVersion stamp = secondVersion compiledMethod timeStamp.

	self deny: firstVersion sourcePointer = secondVersion sourcePointer.
	self deny: firstVersion sourceCode = secondVersion sourceCode.