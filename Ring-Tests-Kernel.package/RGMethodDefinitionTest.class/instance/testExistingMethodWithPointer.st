testing
testExistingMethodWithPointer
	| newMethod |
	
	newMethod:= (OrderedCollection>>#size) asActiveRingDefinition.
									
	self assert: (newMethod parent isNil).
	self assert: (newMethod parentName == #OrderedCollection).
	self assert: (newMethod sourcePointer notNil).
	self assert: (newMethod sourceCode notNil).
	self assert: (newMethod protocol notNil).
	self assert: (newMethod stamp notNil).
	self assert: (newMethod isExtension not)