test examples
testAbs
	"self debug: #testAbs"
	
	self assert: (MethodFinder new load: #((4) 4  (-10) 10 (-3) 3 (2) 2 (-6) 6 (612) 612)) searchForOne asArray equals: #('data1 abs')