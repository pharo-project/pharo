tests
testInstanceInitialization
	self assert: (cache instVarNamed: #maximumSize) = FreeTypeCache defaultMaximumSize.
	self assert: (cache instVarNamed: #used) = 0.
	self assert: (cache instVarNamed: #fontTable) class = cache dictionaryClass.
	self assert: (cache instVarNamed: #fontTable) isEmpty.
	self assert: (cache instVarNamed: #fifo) class = cache fifoClass.
	self assert: (cache instVarNamed: #fifo) isEmpty.
	