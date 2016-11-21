test examples
testMethodFor
	"self debug: #testMethodFor"
	
	self assert: (MethodFinder  methodFor: {#(1). true. #(2). false. #(5). true. #(10). false}) =  '(data1 allMask: 1) (data1 anyMask: 1) (data1 odd) '