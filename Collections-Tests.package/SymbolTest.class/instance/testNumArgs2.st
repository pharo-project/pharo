tests
testNumArgs2
    "TODO: need to be extended to support shrinking and for selectors like #+ " 
	
	self assert: (#test numArgs: 0) = #test.
	self assert: (#test numArgs: 1) = #test:.
	self assert: (#test numArgs: 2) = #test:with:.
	self assert: (#test numArgs: 3) = #test:with:with:.
	

	self assert: (#test: numArgs: 0) = #test:.
	self assert: (#test: numArgs: 1) = #test:.
	self assert: (#test: numArgs: 2) = #test:with:.
	self assert: (#test: numArgs: 3) = #test:with:with:.
	
	self assert: (#test:with: numArgs: 0) = #test:with:.
	self assert: (#test:with: numArgs: 1) = #test:with:.
	self assert: (#test:with: numArgs: 2) = #test:with:.
	self assert: (#test:with: numArgs: 3) = #test:with:with:.
	self assert: (#test:with: numArgs: 4) = #test:with:with:with:.
	
	self assert: (#test:with:with: numArgs: 0) = #test:with:with:.
	self assert: (#test:with:with: numArgs: 1) = #test:with:with:.
	self assert: (#test:with:with: numArgs: 2) = #test:with:with:.
	self assert: (#test:with:with: numArgs: 3) = #test:with:with:.
	self assert: (#test:with:with: numArgs: 4) = #test:with:with:with:.