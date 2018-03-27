"
SUnit tests for objects
"
Class {
	#name : #ObjectTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Objects'
}

{ #category : #private }
ObjectTest >> a [
	self b.
]

{ #category : #private }
ObjectTest >> a1 [
	self b1.
]

{ #category : #private }
ObjectTest >> b [
	self haltIf: #testHaltIf.
]

{ #category : #private }
ObjectTest >> b1 [
	self haltIf: #testasdasdfHaltIf.
]

{ #category : #'assertions - halt' }
ObjectTest >> shouldHaltAfter: aNumber times: aBlock [
	
	self shouldHaltWhen: [ aNumber timesRepeat: aBlock ].
]

{ #category : #'assertions - halt' }
ObjectTest >> shouldHaltWhen: aBlock [

	self should: aBlock raise: Halt.
]

{ #category : #'assertions - halt' }
ObjectTest >> shouldntHaltAfter: aNumber times: aBlock [
	
	self shouldntHaltWhen: [ aNumber timesRepeat: aBlock ].
]

{ #category : #'assertions - halt' }
ObjectTest >> shouldntHaltWhen: aBlock [

	self shouldnt: aBlock raise: Halt.
]

{ #category : #tests }
ObjectTest >> testBecome [	
	"this test should that all the variables pointing to an object are pointing now to another one, and all
      object pointing to the other are pointing to the object"

	| pt1 pt2 pt3 |
	pt1 := 0@0.
	pt2 := pt1.
	pt3 := 100@100.

	pt1 become: pt3.
	self assert: pt2 = (100@100).
	self assert: pt3 = (0@0).
	self assert: pt1 = (100@100).
]

{ #category : #tests }
ObjectTest >> testBecomeForward [	
	"this test should that all the variables pointing to an object are pointing now to another one.
	Not that this inverse is not true. This kind of become is called oneWayBecome in VW"

	| pt1 pt2 pt3 |
	pt1 := 0@0.
	pt2 := pt1.
	pt3 := 100@100.
	pt1 becomeForward: pt3.
	self assert: pt2 = (100@100).
	self assert: pt3 == pt2.
	self assert: pt1 = (100@100)
]

{ #category : #'tests - debugging' }
ObjectTest >> testHaltIf [

	self shouldHaltWhen: [self haltIf: true].
	self shouldntHaltWhen: [self haltIf: false].

	self shouldHaltWhen: [self haltIf: [true]].
	self shouldntHaltWhen: [self haltIf: [false]].

	self shouldHaltWhen: [self haltIf: #testHaltIf].
	self shouldntHaltWhen: [self haltIf: #teadfasdfltIf].

	self shouldHaltWhen: [self a].
	self shouldntHaltWhen: [self a1].

	self shouldHaltWhen: [self haltIf: [:receiver | receiver class = self class]].
	self shouldntHaltWhen: [self haltIf: [:receiver | receiver class ~= self class]].
]

{ #category : #'tests - debugging' }
ObjectTest >> testHaltOnce [
	| anObject block |
	anObject := Object new.
	block :=  [ anObject haltOnce ]. 
	self should: block raise: Halt.
	"block value."
	Halt resetOnce.
]

{ #category : #tests }
ObjectTest >> testInstVarNamed [
	| obj |
	obj := ObjectMockForTest new.
	self assert: (obj instVarNamed: 'variable') isNil.
	obj variable: 1.
	self assert: (obj instVarNamed: 'variable') equals: 1.
	self shouldnt: [ obj instVarNamed: 'variable' ] raise: InstanceVariableNotFound.
	self should: [ obj instVarNamed: 'timoleon' ] raise: InstanceVariableNotFound.
	self
		assert:
			([ obj instVarNamed: 'timoleon' ]
				on: InstanceVariableNotFound
				do: [ :ex | ex instVarName ])
		equals: 'timoleon'
]

{ #category : #tests }
ObjectTest >> testInstVarNamedPut [
	| obj |
	obj := ObjectMockForTest new.
	self assert: (obj instVarNamed: 'variable') isNil.
	obj instVarNamed: 'variable' put: 1.
	self assert: (obj instVarNamed: 'variable') equals: 1.
	self shouldnt: [ obj instVarNamed: 'variable' put: 1 ] raise: InstanceVariableNotFound.
	self should: [ obj instVarNamed: 'timoleon' put: 1 ] raise: InstanceVariableNotFound
]

{ #category : #'tests - introspection' }
ObjectTest >> testReadSlot [
	self assert: (5@3 readSlot: (Point slotNamed: #x)) = 5
]

{ #category : #'tests - introspection' }
ObjectTest >> testReadSlotNamed [
	self assert: (5@3 readSlotNamed: #x) = 5
]

{ #category : #'tests - introspection' }
ObjectTest >> testWriteSlotNamedValue [
	| object |
	object := 5@6.
	
	self assert: (object writeSlotNamed: #x value: 7) = 7.
	self assert: object = (7@6).
]

{ #category : #'tests - introspection' }
ObjectTest >> testWriteSlotValue [
	| object |
	object := 5@6.
	
	self assert: (object writeSlot: (Point slotNamed: #x) value: 7) = 7.
	self assert: object = (7@6).
]
