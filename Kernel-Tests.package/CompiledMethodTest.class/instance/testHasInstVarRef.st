tests - instance variable
testHasInstVarRef
	"self debug: #testHasInstVarRef"
	
	| method  |
	method := self class compiledMethodAt: #readX.
	self assert: (method hasInstVarRef).

	method := self class compiledMethodAt: #readXandY.
	self assert: (method hasInstVarRef).
	
	method := self class compiledMethodAt: #writeX.
	self assert: (method hasInstVarRef).
	
	method := self class compiledMethodAt: #writeXandY.
	self assert: (method hasInstVarRef).
	