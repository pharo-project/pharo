tests
testCreation
	| d |
	d :=  self mockClassA asClassDefinition.
	self assert: d className = #MCMockClassA.
	self assert: d superclassName = #MCMock.
	self assert: d type = #normal.
	self assert: d category = self mockCategoryName.
	self assert: d instVarNames asArray = #('ivar').
	self assert: d classVarNames asArray = #('CVar' 'InitializationOrder').
	self assert: d classInstVarNames asArray = #().
	self assert: d comment isString.
	self assert: d comment = self classAComment.
	self assert: d commentStamp = self mockClassA organization commentStamp