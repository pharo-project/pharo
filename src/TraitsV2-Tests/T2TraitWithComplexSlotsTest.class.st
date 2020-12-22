"
Unit test for traits with complex slots
"
Class {
	#name : #T2TraitWithComplexSlotsTest,
	#superclass : #T2AbstractTest,
	#instVars : [
		'slotClass'
	],
	#category : #'TraitsV2-Tests'
}

{ #category : #'instance creation' }
T2TraitWithComplexSlotsTest >> createSlotClass [
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
T2TraitWithComplexSlotsTest >> testNormalClassWithComplexSlot [
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
T2TraitWithComplexSlotsTest >> testTraitWithComplexSlot [
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
T2TraitWithComplexSlotsTest >> testTraitWithComplexSlotAddClassSlot [
	| t1 c1 |
	self createSlotClass.
	t1 := self newTrait: #T1 with: { #ivar }.
	c1 := self newClass: #C1 with: { #aSlot } uses: { t1 }.
	self assert: (c1 traitComposition includesTrait: t1).
	c1 addClassSlot:  #ivar2 => InstanceVariableSlot.
	self assert: c1 instVarNames equals: #( aSlot ivar ).
	self assert: c1 class instVarNames equals: #( ivar2 ).
	self assert: (c1 traitComposition includesTrait: t1)
]

{ #category : #'instance creation' }
T2TraitWithComplexSlotsTest >> testTraitWithComplexSlotAddSlot [
	| t1 c1 |
	self createSlotClass.
	t1 := self newTrait: #T1 with: { #ivar }.
	c1 := self newClass: #C1 with: { #aSlot } uses: { t1 }.
	self assert: (c1 traitComposition includesTrait: t1).
	c1 addSlot: #ivar2 => InstanceVariableSlot.
	self assert: c1 instVarNames equals: #( aSlot ivar2 ivar ).
	self assert: (c1 traitComposition includesTrait: t1)
]

{ #category : #'instance creation' }
T2TraitWithComplexSlotsTest >> testTraitWithComplexSlotAfter [
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
T2TraitWithComplexSlotsTest >> testTraitWithComplexSlotInSuperclass [
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
T2TraitWithComplexSlotsTest >> testTraitWithComplexSlotUpdatedAfter [
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
T2TraitWithComplexSlotsTest >> testTraitWithComplexSlotUsedInOtherSlot [
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
T2TraitWithComplexSlotsTest >> testTraitWithComplexSlotUsedInOtherSlotInSuperclass [
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

{ #category : #'instance creation' }
T2TraitWithComplexSlotsTest >> testTraitWithComplexSlotUsedInOtherSlotWithoutTraitComposition [
	| t1 t2 c1 obj |
	
	self createSlotClass.
	t1 := self newTrait: #T1 with: { #otherSlot => (slotClass associatedSlotName: #aSlot) }. 
	t2 := self newTrait: #T2 with: { #otherSlot }.
	c1 := self newClass: #C1 with: { #aSlot. } uses: {t1. t2 -- #otherSlot}.

	c1 compile: 'initialize
		aSlot := 0'.
	
	t2 compile: 'doSomething
		otherSlot := ''a'''.
	
	obj := c1 new.
	self assert: (obj instVarNamed: #aSlot) equals: 0.
	obj doSomething.
	self assert: (obj instVarNamed: #aSlot) equals: 1.

]
