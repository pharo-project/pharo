"
Unit test for traits with slots
"
Class {
	#name : 'TraitWithSlotsTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitWithSlotsTest >> createT1 [
	| t1 |
	t1 := self newTrait: #T1 with: 'a b c' asSlotCollection.
	t1
		compile:
			'setValues
		a := 1.
		b := 2.
		c := 3.
	'.
	t1
		compile:
			'getValues
		^ a + b + c
	'.
	^ t1
]

{ #category : 'tests' }
TraitWithSlotsTest >> testAddingStatefulTraitToClassAddsInstanceVariable [

	| t1 c1 obj |
	t1 := self newTrait: #T1 with: #(aSlot).
	c1 := self newClass: #C1 superclass: Object with: #(anotherSlot) traits: {}.

	obj := c1 new.

	self should: [ obj instVarAt: 2 ] raise: PrimitiveFailed.

	c1 := self newClass: #C1 superclass: Object with: #(anotherSlot) traits: {t1}.

	self assert: (obj instVarAt: 2) isNil.
]

{ #category : 'tests' }
TraitWithSlotsTest >> testClassUsingStatefulTraits [

	| t1 c1 |

	t1 := self newTrait: #T1 with: #(aSlot).
	c1 := self newClass: #C1 superclass: Object traits: { t1 }.

	self assertCollection: c1 slots hasSameElements: { c1 slotNamed: #aSlot }
]

{ #category : 'tests' }
TraitWithSlotsTest >> testClassUsingStatefulTraitsAndLocalSlots [

	| t1 c1 |

	t1 := self newTrait: #T1 with: #(aSlot).
	c1 := self newClass: #C1 superclass: Object with: #(anotherSlot) traits: { t1 }.

	self assertCollection: c1 slots hasSameElements: { c1 slotNamed: #aSlot. c1 slotNamed: #anotherSlot }
]

{ #category : 'tests' }
TraitWithSlotsTest >> testDefiningClass [

	| t1 c1 |

	t1 := self createT1.
	c1 := self newClass: #C1 with: 'g h' asSlotCollection traits: t1.

	self assert: (t1 slotNamed: #a) definingClass equals: t1.
	self assert: (t1 slotNamed: #b) definingClass equals: t1.
	self assert: (t1 slotNamed: #c) definingClass equals: t1.

	self assert: (c1 slotNamed: #a) definingClass equals: t1.
	self assert: (c1 slotNamed: #b) definingClass equals: t1.
	self assert: (c1 slotNamed: #c) definingClass equals: t1.
	self assert: (c1 slotNamed: #g) definingClass equals: c1.
	self assert: (c1 slotNamed: #h) definingClass equals: c1
]

{ #category : 'tests' }
TraitWithSlotsTest >> testDiamonProblemIsNotConflict [

	| t1 t2 c1 t3 t4 |
	t1 := self newTrait: #T1 with: #( anSlot ).
	t2 := self newTrait: #T2 with: #( s2 ) traits: { t1 }.
	t3 := self newTrait: #T3 with: #( s3 ) traits: { t1 }.
	t4 := self newTrait: #T4 with: #( s4 ) traits: { t1 }.

	t1 compile: 'm1 ^ anSlot:=42'.

	c1 := self newClass: #C1 with: #( aSlot s2 ) traits: t2 + t3.
	c1 traitComposition: c1 traitComposition + t4.

	self assert: (c1 new perform: #m1) equals: 42
]

{ #category : 'tests' }
TraitWithSlotsTest >> testHavingASlotAlreadyInTheHierarchy [

	| t1 t2 c1 |
	t1 := self newTrait: #T1 with: #( anSlot ).
	t2 := self newTrait: #T2 with: #( s2 ) traits: { t1 }.

	t1 compile: 'm1 ^ anSlot:=42'.

	c1 := self newClass: #C1 with: #( aSlot s2 ) traits: t2 + t1.

	self assert: (c1 new perform: #m1) equals: 42
]

{ #category : 'tests' }
TraitWithSlotsTest >> testOwningClass [

	| t1 c1 |

	t1 := self createT1.
	c1 := self newClass: #C1 with: 'g h' asSlotCollection traits: t1.

	self assert: (t1 slotNamed: #a) owningClass equals: t1.
	self assert: (t1 slotNamed: #b) owningClass equals: t1.
	self assert: (t1 slotNamed: #c) owningClass equals: t1.

	self assert: (c1 slotNamed: #a) owningClass equals: c1.
	self assert: (c1 slotNamed: #b) owningClass equals: c1.
	self assert: (c1 slotNamed: #c) owningClass equals: c1.
	self assert: (c1 slotNamed: #g) owningClass equals: c1.
	self assert: (c1 slotNamed: #h) owningClass equals: c1
]

{ #category : 'tests' }
TraitWithSlotsTest >> testRedefiningSuperclass [

	| t1 c1 t2 c2 |

	t1 := self newTrait: #T1 with: #(a b).
	t2 := self newTrait: #T2 with: #(g h).

	c1 := self newClass: #C1 with: #(c d) traits: t1.
	c2 := self newClass: #C2 superclass: c1 with: #(e) traits: t2.

	self assert: (c2 localSlots collect: [:each | each name]) equals: #(e).
	self assert: (c1 localSlots collect: [:each | each name]) equals: #(c d).
	self assertCollection: (c2 allSlots collect: [:each | each name]) hasSameElements: #(c d a b e g h).
	self assertCollection: (c1 allSlots collect: [:each | each name]) hasSameElements: #(c d a b).

	c1 := self newClass: #C1 with: #(c d) traits: t1.

	self assert: (c2 localSlots collect: [:each | each name]) equals: #(e).
	self assert: (c1 localSlots collect: [:each | each name]) equals: #(c d).
	self assertCollection: (c2 allSlots collect: [:each | each name]) hasSameElements: #(c d a b e g h).
	self assertCollection: (c1 allSlots collect: [:each | each name]) hasSameElements: #(c d a b).
]

{ #category : 'tests' }
TraitWithSlotsTest >> testRedefiningTrait [

| t1 |

t1 := self newTrait: #T1 with: #().
t1 := self newTrait: #T1 with: #(aSlot).

self deny: t1 slots isEmpty
]
