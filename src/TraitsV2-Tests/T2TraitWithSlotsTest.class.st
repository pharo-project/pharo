"
Unit test for traits with slots
"
Class {
	#name : #T2TraitWithSlotsTest,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests'
}

{ #category : #tests }
T2TraitWithSlotsTest >> createT1 [
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

{ #category : #tests }
T2TraitWithSlotsTest >> testClassUsingStatefulTraits [

	| t1 c1 |

	t1 := self newTrait: #T1 with: #(aSlot).
	c1 := self newClass: #C1 superclass: Object with: #() uses: { t1 }. 

	self assertCollection: c1 slots hasSameElements: { c1 slotNamed: #aSlot }.
]

{ #category : #tests }
T2TraitWithSlotsTest >> testClassUsingStatefulTraitsAndLocalSlots [

	| t1 c1 |

	t1 := self newTrait: #T1 with: #(aSlot).
	c1 := self newClass: #C1 superclass: Object with: #(anotherSlot) uses: { t1 }. 

	self assertCollection: c1 slots hasSameElements: { c1 slotNamed: #aSlot. c1 slotNamed: #anotherSlot }
]

{ #category : #tests }
T2TraitWithSlotsTest >> testDefiningClass [

	| t1 c1 |
	
	t1 := self createT1.
	c1 := self newClass: #C1 with: 'g h' asSlotCollection uses: t1.
	
	self assert: (t1 slotNamed: #a) definingClass equals: t1.
	self assert: (t1 slotNamed: #b) definingClass equals: t1.	
	self assert: (t1 slotNamed: #c) definingClass equals: t1.

	self assert: (c1 slotNamed: #a) definingClass equals: t1.
	self assert: (c1 slotNamed: #b) definingClass equals: t1.	
	self assert: (c1 slotNamed: #c) definingClass equals: t1.
	self assert: (c1 slotNamed: #g) definingClass equals: c1.
	self assert: (c1 slotNamed: #h) definingClass equals: c1.

]

{ #category : #tests }
T2TraitWithSlotsTest >> testDiamonProblemIsNotConflict [
	| t1 t2 c1 t3 t4 |
	
	t1 := self newTrait: #T1 with: #(anSlot) uses: {}.
	t2 := self newTrait: #T2 with: #(s2) uses: {t1}.
	t3 := self newTrait: #T3 with: #(s3) uses: {t1}.
	t4 := self newTrait: #T4 with: #(s4) uses: {t1}.

	t1 compile: 'm1 ^ anSlot:=42'.

	c1 := self newClass: #C1 with: #(aSlot s2) uses: t2 + t3.
	c1 traitComposition: c1 traitComposition + t4.
		
	self assert: (c1 new perform: #m1) equals: 42.

]

{ #category : #tests }
T2TraitWithSlotsTest >> testHavingASlotAlreadyInTheHierarchy [
	| t1 t2 c1 |
	
	t1 := self newTrait: #T1 with: #(anSlot) uses: {}.
	t2 := self newTrait: #T2 with: #(s2) uses: {t1}.

	t1 compile: 'm1 ^ anSlot:=42'.

	c1 := self newClass: #C1 with: #(aSlot s2) uses: t2 + t1.
		
	self assert: (c1 new perform: #m1) equals: 42.

]

{ #category : #tests }
T2TraitWithSlotsTest >> testOwningClass [

	| t1 c1 |
	
	t1 := self createT1.
	c1 := self newClass: #C1 with: 'g h' asSlotCollection uses: t1.
	
	self assert: (t1 slotNamed: #a) owningClass equals: t1.
	self assert: (t1 slotNamed: #b) owningClass equals: t1.	
	self assert: (t1 slotNamed: #c) owningClass equals: t1.

	self assert: (c1 slotNamed: #a) owningClass equals: c1.
	self assert: (c1 slotNamed: #b) owningClass equals: c1.	
	self assert: (c1 slotNamed: #c) owningClass equals: c1.
	self assert: (c1 slotNamed: #g) owningClass equals: c1.
	self assert: (c1 slotNamed: #h) owningClass equals: c1.
	


]

{ #category : #tests }
T2TraitWithSlotsTest >> testRedefiningSuperclass [

	| t1 c1 t2 c2 |
	
	t1 := self newTrait: #T1 with: #(a b).
	t2 := self newTrait: #T2 with: #(g h).

	c1 := self newClass: #C1 with: #(c d) uses: t1.
	c2 := self newClass: #C2 superclass: c1 with: #(e) uses: t2.
	
	self assert: (c2 localSlots collect: #name) equals: #(e).
	self assert: (c1 localSlots collect: #name) equals: #(c d).
	self assertCollection: (c2 allSlots collect: #name) hasSameElements: #(c d a b e g h).
	self assertCollection: (c1 allSlots collect: #name) hasSameElements: #(c d a b).	

	c1 := self newClass: #C1 with: #(c d) uses: t1.	

	self assert: (c2 localSlots collect: #name) equals: #(e).
	self assert: (c1 localSlots collect: #name) equals: #(c d).
	self assertCollection: (c2 allSlots collect: #name) hasSameElements: #(c d a b e g h).
	self assertCollection: (c1 allSlots collect: #name) hasSameElements: #(c d a b).	
]

{ #category : #tests }
T2TraitWithSlotsTest >> testRedefiningTrait [

| t1 |

t1 := self newTrait: #T1 with: #().
t1 := self newTrait: #T1 with: #(aSlot).

self deny: t1 slots isEmpty.


]
