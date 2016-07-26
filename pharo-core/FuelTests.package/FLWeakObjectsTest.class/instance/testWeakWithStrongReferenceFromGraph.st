tests
testWeakWithStrongReferenceFromGraph
	"This tests when there are weak objects inside the graph to serialize and there are also strong references from the graph."
	| weak graph obj1 obj2 materializedGraph |
	obj1 := 11.
	obj2 := 'something'.
	weak := WeakArray with: obj1 with: obj2. 
	graph := OrderedCollection with: 40 with: 'aaaa' with: weak with: obj2.
	
	materializedGraph := self resultOfSerializeAndMaterialize: graph.
	self assert: (materializedGraph at: 3) first isNil.
	self assert: (materializedGraph at: 3) second = obj2.
	
	self deny: (self serializationOf: graph includes: 11).
	self assert: (self serializationOf: graph includes: obj2).
	
	 
		

