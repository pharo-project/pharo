test examples
testGreaterThanANumber
	"self debug: #testGreaterThanANumber"
	
	self 
		assert: (MethodFinder new load: {#(7). true.   #(4.1).  true.   #(1.5). false}) searchForOne asArray 
		equals: #('data1 >= 4.1').
		
	self assert: (MethodFinder new load: {#(4 3). true.  #(-7 3). false.  #(5 1). true.  #(5 5). false}) searchForOne asArray 
		equals: #('data1 > data2').
	self assert: (MethodFinder new load: #((36) 7   (50) 10)) searchForOne asSortedCollection asArray equals: #( 'data1 // 5' 'data1 quo: 5')