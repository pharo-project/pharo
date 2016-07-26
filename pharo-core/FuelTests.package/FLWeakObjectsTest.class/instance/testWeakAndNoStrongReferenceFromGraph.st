tests
testWeakAndNoStrongReferenceFromGraph
	"This tests when there are weak objects inside the graph to serialize but there are no strong references from the graph."
	| weak graph obj1 obj2 materializedGraph |
	obj1 := 11.
	obj2 := 'something'.
	weak := WeakArray with: obj1 with: obj2. 
	graph := OrderedCollection with: 40 with: 'aaaa' with: weak.
	
	materializedGraph := self resultOfSerializeAndMaterialize: graph.
	self assert: (materializedGraph at: 3) first isNil.
	self assert: (materializedGraph at: 3) second isNil.
	
	self deny: (self serializationOf: graph includes: 11).
	self deny: (self serializationOf: graph includes: 'something').
	
	 
		
