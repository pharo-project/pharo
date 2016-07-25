basic tests
testDo
	"self run: #testDo" 

	| heap coll |
	heap := Heap withAll: #(1 3 5).
	coll := OrderedCollection new.
	
	heap do: [:each | coll add: each].
	
	self assert: coll = #(1 3 5) asOrderedCollection.
