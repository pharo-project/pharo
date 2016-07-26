test examples
testDegreeSin
	"self debug: #testDegreeSin"
	
	self assert: (MethodFinder new load: #( (0) 0  (30) 0.5  (45) 0.707106  (90) 1)) searchForOne asArray = #('data1 degreeSin') 