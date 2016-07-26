tests
testNumArgsUnary
	self assert: ('name' numArgs = 0).

	self assert: ('name_' numArgs = 0).
	self assert: ('_name' numArgs = 0).
	self assert: ('na_me' numArgs = 0).

	self assert: ('name ' numArgs = -1).
	self assert: (' name' numArgs = -1).
	self assert: ('na me' numArgs = -1).
	
	self assert: ('name+' numArgs = -1).
	self assert: ('+name' numArgs = -1).
	self assert: ('na+me' numArgs = -1).
	
	self assert: ('name123' numArgs = 0).
	self assert: ('na123me' numArgs = 0).
	self assert: ('123name' numArgs = -1)