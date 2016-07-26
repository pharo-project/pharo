tests
testNumArgsBinary
	self assert: ('-' numArgs = 1).
	self assert: ('++' numArgs = 1).
	self assert: ('*&@' numArgs = 1).
	
	self assert: (' ++' numArgs = -1).
	self assert: ('+ +' numArgs = -1).
	self assert: ('++ ' numArgs = -1).

	self assert: ('_++' numArgs = -1).
	self assert: ('+_+' numArgs = -1).
	self assert: ('++_' numArgs = -1).
	
	self assert: ('1' numArgs = -1).
	self assert: ('*a' numArgs = -1)