basic tests
testFirst
	"self run: #testFirst"
	| heap | 
	heap := Heap new.
	heap add: 5.
	heap add: 12.
	heap add: 1.
	self assert: heap first = 1.
	heap removeFirst.
	self assert: heap first = 5.