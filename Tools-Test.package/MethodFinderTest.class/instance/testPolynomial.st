test examples
testPolynomial
	"self debug: #testPolynomial"
	
	self assert: (MethodFinder new load: #( ((2 3) 2) 8   ((2 3) 5) 17 )) searchForOne asArray 
		equals: #('data1 polynomialEval: data2').	
	self assert: (MethodFinder new load: #((2) 8   (5) 17 )) searchForOne asArray 
		equals: #('#(2 3) polynomialEval: data1')	
