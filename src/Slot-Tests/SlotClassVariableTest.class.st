Class {
	#name : 'SlotClassVariableTest',
	#superclass : 'SlotClassBuilderTest',
	#category : 'Slot-Tests-ClassBuilder',
	#package : 'Slot-Tests',
	#tag : 'ClassBuilder'
}

{ #category : 'tests' }
SlotClassVariableTest >> testAccessorInstanceVariableSlotGeneratesAccessors [
	| class1 |

	class1 := self make: [ :builder |
		builder
			name: self aClassName;
			superclass: Object ].

	class1 class addSlot: (AccessorInstanceVariableSlot named: #foo).

	self assert: (class1 class canUnderstand: #foo).
	self assert: (class1 class canUnderstand: #foo:)
]

{ #category : 'tests' }
SlotClassVariableTest >> testClassVariableDoesNotDuplicatesSubclassesOfSuperclass [
	"Issue: 13028"
	| class1 class2 |

	class1 := self make: [ :builder |
		builder
			name: self aClassName;
			superclass: Object ].

	class2 := self make: [ :builder |
		builder
			name: self anotherClassName;
			superclass: class1 ].

	self assert: class1 subclasses equals: { class2 }.
	class2 := self make: [ :builder |
		builder
			name: self anotherClassName;
			superclass: class1;
			sharedVariables: #(ASharedVariable) ].
	self assert: class1 subclasses equals: { class2 }
]

{ #category : 'tests' }
SlotClassVariableTest >> testWantsInitializationAddsInitializeSlot [

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
SlotClassVariableTest >> testWantsInitializationSkipInitializeSlotIfAlreadyInHierarchy [
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
