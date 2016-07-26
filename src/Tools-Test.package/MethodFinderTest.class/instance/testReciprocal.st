test examples
testReciprocal
	"self debug: #testReciprocal"
	
	self assert: (MethodFinder new load: #((5) 0.2   (2) 0.5)) searchForOne asArray = #('data1 reciprocal')
