Class {
	#name : #RelationSlotTest,
	#superclass : #TestCase,
	#category : #'Slot-Tests-Examples - Associations'
}

{ #category : #tests }
RelationSlotTest >> testDefinition [

	| s1 s2 |
	
	s1 := ToManyRelationSlot named: #people inverse: #x inClass: #SlotExamplePerson.
	s2 := #people => ToManyRelationSlot inverse: #x inClass: #SlotExamplePerson.
	
	self assert: s1 equals: s2.
	self assert: s1 hasInverse 

]

{ #category : #tests }
RelationSlotTest >> testDefinitionWithClass [

	| s1 s2 |
	
	s1 := #people => ToManyRelationSlot inverse: #x inClass: #SlotExamplePerson.
	s2 := #people => ToManyRelationSlot inverse: #x inClass: SlotExamplePerson.
	
	self assert: s1 equals: s2.
	self assert: s1 hasInverse 

]

{ #category : #tests }
RelationSlotTest >> testDefinitionWithoutInverse [

	| slot |
	
	slot := #people => ToManyRelationSlot inClass: #SlotExamplePerson.
	self deny: slot hasInverse
]

{ #category : #tests }
RelationSlotTest >> testEquals [

	| s1 s2 |
	
	s1 := ToManyRelationSlot named: #slot inverse: #x inClass: #SlotExamplePerson.
	s2 := ToManyRelationSlot named: #slot inverse: #x inClass: #SlotExamplePerson.
	
	self assert: s1 equals: s2.
	self assert: s1 hash equals: s2 hash
]

{ #category : #tests }
RelationSlotTest >> testNotEquals [
	| s1 s2 |
	s1 := ToManyRelationSlot named: #slot inverse: #x inClass: #SlotExamplePerson.
	s2 := ToManyRelationSlot named: #slot inverse: #y inClass: #SlotExamplePerson.

	self deny: s1 equals: s2
]
