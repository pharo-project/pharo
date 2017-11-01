Class {
	#name : #SlotClassVariableTest,
	#superclass : #SlotClassBuilderTest,
	#instVars : [
		'announcement',
		'collectedAnnouncements'
	],
	#category : 'Slot-Tests'
}

{ #category : #tests }
SlotClassVariableTest >> testAccessorInstanceVariableSlotGeneratesAccessors [
	| class1 |	
	
	class1 := self make: [ :builder | 
		builder
			name: self aClassName;
			superclass: Object ].
		
	class1 class addSlot: (AccessorInstanceVariableSlot named: #foo).

	self assert: (class1 class canUnderstand: #foo).
	self assert: (class1 class canUnderstand: #foo:).
]

{ #category : #tests }
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
			sharedVariablesFromString: 'ASharedVariable' ].
	self assert: class1 subclasses equals: { class2 }.

]

{ #category : #tests }
SlotClassVariableTest >> testSlotIsPersistedAfterRebuildOfClass [
	| class1 |	
	
	class1 := self make: [ :builder | 
		builder
			name: self aClassName;
			superclass: Object ].
		
	class1 class addSlot: (AccessorInstanceVariableSlot named: #Foo).

	self assert: class1 class slots size equals: 1.
	self assert: (class1 class slots at:1) name equals: #Foo.
	self assert: (class1 class slots at:1) class equals: AccessorInstanceVariableSlot.
	
	class1 := self make: [ :builder | 
		builder
			name: self aClassName;
			superclass: Object;
			copyClassSlotsFromExistingClass ].

	self assert: class1 class slots size equals: 1.
	self assert: (class1 class slots at:1) name equals: #Foo.
	self assert: (class1 class slots at:1) class equals: AccessorInstanceVariableSlot.

]
