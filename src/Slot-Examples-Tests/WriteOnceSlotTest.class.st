"
Automated tests for WriteOnceSlot
"
Class {
	#name : 'WriteOnceSlotTest',
	#superclass : 'SlotSilentTest',
	#category : 'Slot-Examples-Tests-Base',
	#package : 'Slot-Examples-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
WriteOnceSlotTest >> testClassSide [
	"test that we can change a normal slot into a WriteOnceSlot. Tests class side slot init"
	| slot |
	slot := #slot1 => InstanceVariableSlot.
	aClass := self make: [ :builder | builder classSlots: {slot} ].
	slot := #slot1 => WriteOnceSlot.
	aClass := self make: [ :builder | builder classSlots: {slot} ].

	self assert: (aClass class hasSlotNamed: #slot1)
]

{ #category : 'tests' }
WriteOnceSlotTest >> testRead [
	| slot |
	<ignoreNotImplementedSelectors: #(slot1)>
	slot := #slot1 => WriteOnceSlot.
	aClass := self make: [ :builder | builder slots: {slot} ].

	"now compile an accessor and read"
	self compileAccessorsFor: slot.
	self assert: aClass new slot1 equals: nil
]

{ #category : 'tests' }
WriteOnceSlotTest >> testWantsInitializationAddsInitializeSlot [

	| class1 |
	class1 := self make: [ :builder |
		          builder
			          name: self aClassName;
			          superclass: Object ].

	class1 addSlot: #foo => WriteOnceSlot new.

	self assert: class1 slots size equals: 1.

	self assert: (class1 >> #initialize hasLiteral: #initializeSlots:).
	"validate 'self class initializeSlots: self' is the first message sent"
	self assert: (class1 >> #initialize) literals first equals: #class.
	self assert: (class1 >> #initialize) literals second equals: #initializeSlots:
]

{ #category : 'tests' }
WriteOnceSlotTest >> testWantsInitializationSkipInitializeSlotIfAlreadyInHierarchy [
	| class1 class2 |

	class1 := self make: [ :builder |
		builder
			name: self aClassName;
			superclass: Object;
			slots: {
				WriteOnceSlot new
					name: #foo;
					yourself } ].

	self assert: class1 slots size equals: 1.
	self assert: ((class1 >> #initialize) hasLiteral: #initializeSlots:).

	class2 := self make: [ :builder |
		builder
			name: self anotherClassName;
			superclass: class1;
			slots: {
				WriteOnceSlot new
					name: #bar;
					yourself } ].

	self assert: class2 slots size equals: 1.
	class2
		compiledMethodAt: #initialize
		ifPresent: [ self fail: 'In case the super method already call #initializeSlots:, we don''t want to generate an initialize method' ]
]

{ #category : 'tests' }
WriteOnceSlotTest >> testWriteAndRead [
	| slot object |
	<ignoreNotImplementedSelectors: #(slot1: slot1)>
	slot := #slot1 => WriteOnceSlot.
	aClass := self make: [ :builder | builder slots: {slot} ].

	"now compile an accessor and read"
	self compileAccessorsFor: slot.
	object := aClass new.
	self assert: object slot1 equals: nil.
	object slot1: 3.
	self assert: object slot1 equals: 3
]

{ #category : 'tests' }
WriteOnceSlotTest >> testWriteTwice [
	| slot object |
	<ignoreNotImplementedSelectors: #(slot1: slot1)>
	slot := #slot1 => WriteOnceSlot.
	aClass := self make: [ :builder | builder slots: {slot} ].

	"now compile an accessor and read"
	self compileAccessorsFor: slot.
	object := aClass new.
	self assert: object slot1 equals: nil.
	"write first time"
	object slot1: 3.
	self assert: object slot1 equals: 3.
	"second write is error"
	self should: [ object slot1: 27 ] raise: Error
]
