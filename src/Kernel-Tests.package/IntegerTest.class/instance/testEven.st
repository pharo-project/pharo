tests - basic
testEven
	
	self deny: (1073741825 even).
	self assert: (1073741824  even).
	