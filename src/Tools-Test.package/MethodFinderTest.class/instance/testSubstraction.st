test examples
testSubstraction
	"self debug: #testSubstraction"
	
	self assert: (MethodFinder new load: #((14 3) 11  (-10 5) -15  (4 -3) 7)) searchForOne asArray equals: #('data1 - data2').
	
	self assert: (MethodFinder new load: #(((12 4 8)) 24  ((1 3 6)) 10 )
	) searchForOne asArray  equals: #('data1 sum') 