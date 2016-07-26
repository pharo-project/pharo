test examples
testAllNumbers
	"self debug: #testAllNumbers"	

	self assert: (MethodFinder new load: #((5) 0.2   (2) 0.5)) allNumbers.
	self deny: (MethodFinder new load: #(('' $A) 'A')) allNumbers