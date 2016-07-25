test examples
testPower
	"self debug: #testPower"
	
	self assert:(MethodFinder new load: #((7) 2   (4) 2 )) searchForOne asArray equals: #('^ 2')