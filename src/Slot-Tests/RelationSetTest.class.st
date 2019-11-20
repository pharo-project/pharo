Class {
	#name : #RelationSetTest,
	#superclass : #TestCase,
	#category : #'Slot-Tests-Examples - Associations'
}

{ #category : #tests }
RelationSetTest >> testCopy [
	"A copy of an RelationSet should give us a regular Set."

	| movie |
	movie := SlotExampleMovie new.
	self assert: movie actors isCollection.
	self assert: movie actors copy isCollection.
	self deny: movie actors copy class equals: RelationSet
]

{ #category : #tests }
RelationSetTest >> testSelect [

	| movie result |
	
	movie := SlotExampleMovie named: 'Thelma and Louise'.
	movie
		addActor: (SlotExamplePerson named: 'P1') ;
		addActor: (SlotExamplePerson named: 'P2').

	result := movie actors select: [ :e | e name = 'P1' ].
	self assert: result class equals: Set.
	self assert: result size equals: 1.
	self assert: result anyOne name equals: 'P1'.
]
