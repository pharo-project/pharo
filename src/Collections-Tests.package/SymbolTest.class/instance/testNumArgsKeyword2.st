tests
testNumArgsKeyword2
	self assert: ('name:with:' numArgs = 2).

	self assert: ('_name:with:' numArgs = 2).
	self assert: ('na_me:with:' numArgs = 2).
	self assert: ('name_:with:' numArgs = 2).
	
	self assert: ('name:_with:' numArgs = 2).
	self assert: ('name:wi_th:' numArgs = 2).
	self assert: ('name:with_:' numArgs = 2).
	
	self assert: ('name:with: ' numArgs = -1).
	self assert: ('name: with:' numArgs = -1).
	self assert: (' name:with:' numArgs = -1).
	
	self assert: ('name+:with:' numArgs = -1).
	self assert: ('+name:with:' numArgs = -1).
	self assert: ('na+me:with:' numArgs = -1).
	
	self assert: ('123name:with:' numArgs = -1).
	self assert: ('name123:with:' numArgs = 2).
	self assert: ('name:123with:' numArgs = -1).
	self assert: ('name:with123:' numArgs = 2)