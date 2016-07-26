test examples
testIsOdd
	"self debug: #testIsOdd"
	
	self assert: (MethodFinder new load: {#(1). true. #(2). false. #(5). true. #(10). false}) searchForOne asArray = #('data1 odd')