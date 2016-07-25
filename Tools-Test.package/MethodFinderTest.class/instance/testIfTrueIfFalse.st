test examples
testIfTrueIfFalse
	"self debug: #testIfTrueIfFalse"
	
	self assert: (MethodFinder new load:  { { true. [3]. [4]}. 3.  { false. [0]. [6]}. 6}) searchForOne asArray = #('data1 ifTrue: data2 ifFalse: data3') 