Class {
	#name : #T2TraitWithComplexSlots,
	#superclass : #T2AbstractTest,
	#instVars : [
		'slotClass'
	],
	#category : #'TraitsV2-Tests'
}

{ #category : #'instance creation' }
T2TraitWithComplexSlots >> createSlotClass [
	slotClass := self newClass: #TestSlot superclass: IndexedSlot with: #(associatedSlotName) uses: #().
	slotClass class compile: 'associatedSlotName: aName
		^ self new
			associatedSlotName: aName;
			yourself'.
			
	slotClass compile: 'associatedSlotName: aName
		associatedSlotName:=aName'.
	
	slotClass compile: 'write: aValue to: anObject
		super write: aValue to: anObject.
		anObject instVarNamed: associatedSlotName put: (anObject instVarNamed: associatedSlotName) + 1.
		'
]

{ #category : #'instance creation' }
T2TraitWithComplexSlots >> testNormalClassWithComplexSlot [
	| c1 obj |
	
	self createSlotClass.
	c1 := self newClass: #C1 with: { #aSlot. #otherSlot => (slotClass associatedSlotName: #aSlot) } uses: #().
	c1 compile: 'initialize
		aSlot := 0'.
	c1 compile: 'doSomething
		otherSlot := ''a'''.
	
	obj := c1 new.
	self assert: (obj instVarNamed: #aSlot) equals: 0.
	obj doSomething.
	self assert: (obj instVarNamed: #aSlot) equals: 1.

]

{ #category : #'instance creation' }
T2TraitWithComplexSlots >> testTraitWithComplexSlot [
	| t1 c1 obj |
	
	self createSlotClass.
	t1 := self newTrait: #T1 with: { #otherSlot => (slotClass associatedSlotName: #aSlot) }. 
	c1 := self newClass: #C1 with: { #aSlot. } uses: {t1}.

	c1 compile: 'initialize
		aSlot := 0'.
	c1 compile: 'doSomething
		otherSlot := ''a'''.
	
	obj := c1 new.
	self assert: (obj instVarNamed: #aSlot) equals: 0.
	obj doSomething.
	self assert: (obj instVarNamed: #aSlot) equals: 1.

]

{ #category : #'instance creation' }
T2TraitWithComplexSlots >> testTraitWithComplexSlotAfter [
	| t1 c1 obj |
	
	self createSlotClass.
	t1 := self newTrait: #T1 with: {}. 
	c1 := self newClass: #C1 with: { #aSlot. } uses: {t1}.

	c1 compile: 'initialize
		aSlot := 0'.

	t1 := self newTrait: #T1 with: { #otherSlot => (slotClass associatedSlotName: #aSlot) }. 

	c1 compile: 'doSomething
		otherSlot := ''a'''.
	
	obj := c1 new.
	self assert: (obj instVarNamed: #aSlot) equals: 0.
	obj doSomething.
	self assert: (obj instVarNamed: #aSlot) equals: 1.

]

{ #category : #'instance creation' }
T2TraitWithComplexSlots >> testTraitWithComplexSlotInSuperclass [
	| t1 c1 c2 obj |
	
	self createSlotClass.
	t1 := self newTrait: #T1 with: { #otherSlot => (slotClass associatedSlotName: #aSlot) }. 
	c1 := self newClass: #C1 with: { #aSlot. } uses: {t1}.
	c2 := self newClass: #C2 superclass: c1 with: #() uses: #().

	c1 compile: 'initialize
		aSlot := 0'.
	t1 compile: 'doSomething
		otherSlot := ''a'''.
	
	obj := c2 new.
	self assert: (obj instVarNamed: #aSlot) equals: 0.
	obj doSomething.
	self assert: (obj instVarNamed: #aSlot) equals: 1.

]

{ #category : #'instance creation' }
T2TraitWithComplexSlots >> testTraitWithComplexSlotUpdatedAfter [
	| t1 c1 obj |
	
	self createSlotClass.
	t1 := self newTrait: #T1 with: {}. 
	c1 := self newClass: #C1 with: { #aSlot. } uses: {t1}.

	t1 := self newTrait: #T1 with: { #otherSlot => (slotClass associatedSlotName: #aSlot) }. 

	c1 compile: 'initialize
		aSlot := 0'.
	c1 compile: 'doSomething
		otherSlot := ''a'''.
	
	obj := c1 new.
	self assert: (obj instVarNamed: #aSlot) equals: 0.
	obj doSomething.
	self assert: (obj instVarNamed: #aSlot) equals: 1.

]

{ #category : #'instance creation' }
T2TraitWithComplexSlots >> testTraitWithComplexSlotUsedInOtherSlot [
	| t1 t2 c1 obj |
	
	self createSlotClass.
	t1 := self newTrait: #T1 with: { #otherSlot => (slotClass associatedSlotName: #aSlot) }. 
	t2 := self newTrait: #T2 with: { #otherSlot }.
	c1 := self newClass: #C1 with: { #aSlot. } uses: {t1. t2 asTraitComposition -- #otherSlot}.

	c1 compile: 'initialize
		aSlot := 0'.
	
	t2 compile: 'doSomething
		otherSlot := ''a'''.
	
	obj := c1 new.
	self assert: (obj instVarNamed: #aSlot) equals: 0.
	obj doSomething.
	self assert: (obj instVarNamed: #aSlot) equals: 1.

]

{ #category : #'instance creation' }
T2TraitWithComplexSlots >> testTraitWithComplexSlotUsedInOtherSlotInSuperclass [
	| t1 t2 c1 c2 obj |
	
	self createSlotClass.
	t1 := self newTrait: #T1 with: { #otherSlot => (slotClass associatedSlotName: #aSlot) }. 
	t2 := self newTrait: #T2 with: { #otherSlot }.
	c1 := self newClass: #C1 with: { #aSlot. } uses: {t1. t2 asTraitComposition -- #otherSlot}.

	c1 compile: 'initialize
		aSlot := 0'.
	
	t2 compile: 'doSomething
		otherSlot := ''a'''.
	
	c2 := self newClass: #C2 superclass: c1 with: #() uses: #().
	
	obj := c2 new.
	self assert: (obj instVarNamed: #aSlot) equals: 0.
	obj doSomething.
	self assert: (obj instVarNamed: #aSlot) equals: 1.

]
