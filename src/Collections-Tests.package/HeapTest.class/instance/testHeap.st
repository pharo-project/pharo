basic tests
testHeap
	"self run: #testHeap"

	| heap | 
	heap := Heap new.
	self assert: heap isHeap.
	
	self assert: heap isEmpty.
	heap add: 1.
	self assert: heap isEmpty not
	
