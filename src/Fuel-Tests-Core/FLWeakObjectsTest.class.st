"
SUnit tests for fuel serialization of weak objects
"
Class {
	#name : #FLWeakObjectsTest,
	#superclass : #FLSerializationTest,
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #tests }
FLWeakObjectsTest >> testAssociationWithWeak [
	"This tests an association with weak values. InstVar1 is a fixed instance variable, hence they are strong and hence should not be replaed by nil. Instead, 'weak' is stored in the variable part of FLWeakClassMock, so it is weak and so it should have been replaced by nil."

	| weak association materializedAssociation |
	weak := FLWeakClassMock new: 1.
	weak instVar1: 'nonWeak'.
	weak basicAt: 1 put: 'weak'.
	association := Association key: #foo value: weak.

	materializedAssociation := self resultOfSerializeAndMaterialize: association.
	self assert: materializedAssociation value instVar1 isNil not.
	self assert: (materializedAssociation value basicAt: 1) isNil.
	self assert: materializedAssociation key equals: #foo
]

{ #category : #tests }
FLWeakObjectsTest >> testAssociationWithWeakFinalizationList [
	"This tests an association with WeakFInalizationList as its value."

	| weak association materializedAssociation object list |
	object := Object new.
	list := WeakFinalizationList new.
	weak := WeakFinalizerItem new list: list object: object executor: nil.
	association := Association key: #foo value: weak.

	materializedAssociation := self resultOfSerializeAndMaterialize: association.
	"Both, 'executor' and 'list' are fixed instance variables, hence they are strong and hence should not be replaced by nil. Instead, 'object' is stored in the variable part of WeakFinalizationItem, so it is weak and so it should have been replaced by nil."
	self assert: materializedAssociation value executor isNil.
	self assert: materializedAssociation value list isNil not.
	self assert: materializedAssociation value object isNil.
	self assert: materializedAssociation key equals: #foo
]

{ #category : #tests }
FLWeakObjectsTest >> testWeakAndNoStrongReferenceFromGraph [
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
	
	 
		

]

{ #category : #tests }
FLWeakObjectsTest >> testWeakWithStrongReferenceFromGraph [
	"This tests when there are weak objects inside the graph to serialize and there are also strong references from the graph."

	| weak graph obj1 obj2 materializedGraph |
	obj1 := 11.
	obj2 := 'something'.
	weak := WeakArray with: obj1 with: obj2.
	graph := OrderedCollection
		with: 40
		with: 'aaaa'
		with: weak
		with: obj2.

	materializedGraph := self resultOfSerializeAndMaterialize: graph.
	self assert: (materializedGraph at: 3) first isNil.
	self assert: (materializedGraph at: 3) second equals: obj2.

	self deny: (self serializationOf: graph includes: 11).
	self assert: (self serializationOf: graph includes: obj2)
]

{ #category : #tests }
FLWeakObjectsTest >> testWeakWithStrongReferenceFromGraph2 [
	"This tests when there are weak objects inside the graph to serialize and there are also strong references from the graph."

	| weak graph obj1 obj2 materializedGraph |
	obj1 := 11.
	obj2 := 'something'.
	weak := WeakArray with: obj1 with: obj2.
	graph := OrderedCollection
		with: 40
		with: 'aaaa'
		with: weak
		with: obj2
		with: (Point x: weak y: weak).

	materializedGraph := self resultOfSerializeAndMaterialize: graph.
	self assert: (materializedGraph at: 3) first isNil.
	self assert: (materializedGraph at: 3) second equals: obj2.

	self deny: (self serializationOf: graph includes: 11).
	self assert: (self serializationOf: graph includes: obj2)
]
