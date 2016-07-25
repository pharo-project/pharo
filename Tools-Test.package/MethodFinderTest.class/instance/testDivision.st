test examples
testDivision
	"self debug: #testDivision"
	
	self 
		assert: (MethodFinder new load: #((7 5) 2   (4 5) 4   (-9 4) 3)) searchForOne asArray 
		equals: #('data1 \\ data2').
	self assert:
		(MethodFinder new load: #((35) 3   (17) 1   (5) 5)) searchForOne asArray 
		equals: #('data1 \\ 8').
	self assert: (MethodFinder new load: #((12 4 8) 2  (1 3 6) 2  (5 2 16) 8)
	) searchForOne asArray equals:  #().