Class {
	#name : #SlotMigrationTest,
	#superclass : #SlotSilentTest,
	#instVars : [
		'instance'
	],
	#category : #'Slot-Tests-ClassBuilder'
}

{ #category : #tests }
SlotMigrationTest >> testAddSharedVariableKeepSubclasses [
	"Reproduces fogbugz case 13028"

	"Define original hierarchy"

	aClass := self
		make: [ :builder | 
			builder
				name: self aClassName;
				superclass: Object ].

	anotherClass := self
		make: [ :builder | 
			builder
				name: self anotherClassName;
				superclass: aClass ].

	self assert: aClass subclasses size equals: 1.
	self assert: aClass subclasses anyOne identicalTo: anotherClass.

	"Add a shared variable"
	aClass := self
		make: [ :builder | 
			builder
				name: self aClassName;
				superclass: Object;
				sharedVariablesFromString: 'Var' ].

	self assert: aClass subclasses size equals: 1.
	self assert: aClass subclasses anyOne identicalTo: anotherClass
]

{ #category : #tests }
SlotMigrationTest >> testAddSlotAndMigrate [
	"We create a class without slots and create an instance; then we add a slot and check the instance can hold a value."
	aClass := self makeWithLayout: FixedLayout.
	
	instance := aClass new.
	
	"now we extend it with slots.  The class definition should've changed"
	self makeWithLayout: FixedLayout andSlots: { #aSlot }.
	
	"it should have an instance variable"
	instance instVarAt: 1 put: 125.
	aClass classLayout checkIntegrity
]

{ #category : #tests }
SlotMigrationTest >> testAddSlotPropagateAndMigrate [
	"We create a class without slots and a subclass, and create an instance of the latter; then we add a slot to superclass and check the instance can hold a value."

	| superclass subclass |
	superclass := self makeWithLayout: FixedLayout.
	subclass := self make: [ :builder |
		builder 
			superclass: superclass;
			name: self anotherClassName.
		 ].
	
	superclass classLayout checkIntegrity.
	subclass classLayout checkIntegrity.
	
	instance := subclass new.
	
	"now we extend it with slots.  The class definition should've changed"
	superclass := self makeWithLayout: FixedLayout andSlots: { #aSlot }.
	
	"it should have an instance variable"
	instance instVarNamed: 'aSlot' put: 42.
	self assert: (instance instVarNamed: 'aSlot') equals: 42.
	superclass classLayout checkIntegrity.
	subclass classLayout checkIntegrity
]

{ #category : #tests }
SlotMigrationTest >> testAddTestSlot [
	"We create a class without slots and create an instance; then we add a slot and check the instance can hold a value."
	aClass := self makeWithLayout: FixedLayout andSlots: { #aSlot }.
	
	instance := aClass new.
	instance instVarAt: 1 put: 125.
	"now we extend it with a test slots.  The class definition should've changed"
	self makeWithLayout: FixedLayout andSlots: {#anotherSlot => ExampleSlotWithState. #aSlot. }.
	
	"It should not have move the first instance as TestSlot size = 0"
	self assert: (instance instVarAt: 1) equals: 125.
	aClass classLayout checkIntegrity
]

{ #category : #tests }
SlotMigrationTest >> testChangeLayoutTypeFromByte [

	aClass := self makeWithLayout: ByteLayout.
	"Change the layout of the class from bytes to pointer"
	aClass := self 
		makeWithLayout: FixedLayout 
		andSlots: { #id. #name }.
	
	aClass classLayout checkIntegrity.
	
	self deny: aClass classLayout isBits.
	self assert: aClass instVarNames equals: { #id. #name }
]

{ #category : #tests }
SlotMigrationTest >> testChangeLayoutTypeToByte [
	aClass := self makeWithLayout: FixedLayout andSlots: {#id . #name}.
	"Change the layout of the class from pointer to bytes"
	aClass := self makeWithLayout: ByteLayout.

	aClass classLayout checkIntegrity.

	self assert: aClass classLayout isBits.
	self assertEmpty: aClass instVarNames
]

{ #category : #tests }
SlotMigrationTest >> testChangeSuperclass [
	"Define original hierarchy"

	aClass := self
		make: [ :builder | 
			builder
				name: self aClassName;
				superclass: Object ].

	anotherClass := self
		make: [ :builder | 
			builder
				name: self anotherClassName;
				superclass: aClass ].

	self assert: aClass subclasses size equals: 1.
	self assert: aClass subclasses anyOne identicalTo: anotherClass.
	self assert: anotherClass superclass identicalTo: aClass.

	"Change to a new superclass"
	yetAnotherClass := self
		make: [ :builder | 
			builder
				name: self yetAnotherClassName;
				superclass: Object ].

	anotherClass := self
		make: [ :builder | 
			builder
				name: self anotherClassName;
				superclass: yetAnotherClass ].

	self assertEmpty: aClass subclasses.
	self assert: yetAnotherClass subclasses size equals: 1.
	self assert: yetAnotherClass subclasses anyOne identicalTo: anotherClass.
	self assert: anotherClass superclass identicalTo: yetAnotherClass
]

{ #category : #tests }
SlotMigrationTest >> testChangingFormatKeepsMethod [

	aClass := self make: [ :builder | 
		builder 
			name: self aClassName;
			layoutClass: FixedLayout ].

	aClass compile: 'a ^self'.
	self assert: (aClass methodDictionary includesKey: #a).
	
	anotherClass := self make: [ :builder | 
		builder 
			name: self aClassName;
			layoutClass: VariableLayout .
			].

	self assert: (anotherClass methodDictionary includesKey: #a).
	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity
]

{ #category : #tests }
SlotMigrationTest >> testMigrateTestSlot [
	"We create a class with a Test slot, so its  value is shared between instances"
	| instance2 |
	aClass := self makeWithLayout: FixedLayout andSlots: { #aSlot => ExampleSlotWithState. }.
	
	instance := aClass new.
	instance2 := aClass new.
	(aClass slotNamed: #aSlot) write: 125 to: instance2.
	"now we change the TestSlot with a InstanceVariableSlot. Both instances should both shared the same value in this slot."
	self makeWithLayout: FixedLayout andSlots: { #aSlot }.
	
	self assert: ((aClass slotNamed: #aSlot) read: instance) equals: 125.
	self assert: ((aClass slotNamed: #aSlot) read: instance2) equals: 125.
	aClass classLayout checkIntegrity
]

{ #category : #tests }
SlotMigrationTest >> testRedefineSuperclass [
	"This case reproduces a MNU found loading Moose."

	| superclass class |
	superclass := self makeWithLayout: FixedLayout.
	class := self make: [ :builder |
		builder 
			superclass: superclass;
			layoutClass: ByteLayout;
			name: self anotherClassName.
		 ].

	self makeWithLayout: FixedLayout.

	superclass classLayout checkIntegrity.
	class classLayout checkIntegrity
]

{ #category : #tests }
SlotMigrationTest >> testRemoveSlotAndMigrate [
	"We create a class with a slot and create an instance; then we remove the slot and check the instance doesn't hold the value."
	aClass := self makeWithLayout: FixedLayout andSlots: { #aSlot }.

	instance := aClass new.
	"it should have an instance variable"
	instance instVarAt: 1 put: 125.
	
	"now we extend it with slots.  The class definition should've changed"
	self makeWithLayout: FixedLayout.
	
	self should: [ instance instVarAt: 1 ] raise: Error.
	aClass classLayout checkIntegrity
]

{ #category : #tests }
SlotMigrationTest >> testRemoveTestSlot [
	aClass := self makeWithLayout: FixedLayout andSlots: {#anotherSlot => ExampleSlotWithState. #aSlot }.
	
	instance := aClass new.
	instance instVarAt: 1 put: 125.
	"now we remove the TestSlot from the class definition. As testSlot has a size of 0, the object should not change."
	self makeWithLayout: FixedLayout andSlots: {#aSlot. }.
	

	self assert: (instance instVarAt: 1) equals: 125.
	aClass classLayout checkIntegrity
]

{ #category : #tests }
SlotMigrationTest >> testReshapeByteVariableToPointerPropagatesToDeepHierarchy [

	"create the original hierarchy"
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			superclass: Object;
			layoutClass: ByteLayout ]. 

	anotherClass := self make: [ :builder |
		builder 
			name:self anotherClassName;
			superclass: aClass;
			layoutClass: ByteLayout ].

	yetAnotherClass := self make: [ :builder |
		builder 
			name:self yetAnotherClassName;
			superclass: anotherClass;
			layoutClass: ByteLayout ].

	yetYetAnotherClass := self make: [ :builder |
		builder 
			name:self yetYetAnotherClassName;
			superclass: yetAnotherClass;
			layoutClass: ByteLayout ].

	"all the classes in the hierarchy should be of type variable byte"
	{ aClass. anotherClass. yetAnotherClass. yetYetAnotherClass } do: [ :each | 
		each classLayout checkIntegrity.
		self assert: each isVariable.
		self assert: each isBytes. ].
	
	"change the top superclass"	
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			superclass: Object;
			layoutClass: FixedLayout;
			slots: { #x } ]. 

	"all the classes in the hierarchy should be reshaped as fixed"
	{ aClass. anotherClass. yetAnotherClass. yetYetAnotherClass } do: [ :each | 
		each classLayout checkIntegrity.
		self assert: each isFixed description: each name, ' was not reshaped.'.
		self assert: each isPointers description: each name, ' was not reshaped.' ]
]

{ #category : #tests }
SlotMigrationTest >> testReshapeClassPropagatesToDeepHierarchy [
	"Test reshaping a class which is head of a hierarchy of 4 levels"

	"level 1"

	aClass := self make: [ :builder | builder name: self aClassName ].

	self assertEmpty: aClass subclasses.
	aClass classLayout checkIntegrity.

	"level 2"
	anotherClass := self
		make: [ :builder | 
			builder
				superclass: aClass;
				name: self anotherClassName ].

	self assert: aClass subclasses equals: {anotherClass}.
	self assertEmpty: anotherClass subclasses.
	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.

	"level 3"
	yetAnotherClass := self
		make: [ :builder | 
			builder
				superclass: anotherClass;
				name: self yetAnotherClassName ].

	self assert: aClass subclasses equals: {anotherClass}.
	self assert: anotherClass subclasses equals: {yetAnotherClass}.
	self assertEmpty: yetAnotherClass subclasses.
	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.
	yetAnotherClass classLayout checkIntegrity.

	"level 4"
	yetYetAnotherClass := self
		make: [ :builder | 
			builder
				superclass: yetAnotherClass;
				name: self yetYetAnotherClassName ].


	self assert: aClass subclasses equals: {anotherClass}.
	self assert: anotherClass subclasses equals: {yetAnotherClass}.
	self assert: yetAnotherClass subclasses equals: {yetYetAnotherClass}.
	self assertEmpty: yetYetAnotherClass subclasses.
	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.
	yetAnotherClass classLayout checkIntegrity.
	yetYetAnotherClass classLayout checkIntegrity.

	"reshape level 1"
	aClass := self
		make: [ :builder | 
			builder
				name: self aClassName;
				slots: #(x) ].

	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.
	yetAnotherClass classLayout checkIntegrity.
	yetYetAnotherClass classLayout checkIntegrity
]

{ #category : #tests }
SlotMigrationTest >> testReshapePointerToByteVariablePropagatesToDeepHierarchy [

	"create the original hierarchy"
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			superclass: Object;
			layoutClass: FixedLayout ]. 

	anotherClass := self make: [ :builder |
		builder 
			name:self anotherClassName;
			superclass: aClass;
			layoutClass: FixedLayout ].

	yetAnotherClass := self make: [ :builder |
		builder 
			name:self yetAnotherClassName;
			superclass: anotherClass;
			layoutClass: FixedLayout ].

	yetYetAnotherClass := self make: [ :builder |
		builder 
			name:self yetYetAnotherClassName;
			superclass: yetAnotherClass;
			layoutClass: FixedLayout ].

	"all the classes in the hierarchy should be of type fixed pointers"
	{ aClass. anotherClass. yetAnotherClass. yetYetAnotherClass } do: [ :each | 
		each classLayout checkIntegrity.
		self assert: each isFixed.
		self assert: each isPointers. ].
	
	"change the top superclass"	
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			superclass: Object;
			layoutClass: ByteLayout ]. 

	"all the classes in the hierarchy should be reshaped as fixed"
	{ aClass. anotherClass. yetAnotherClass. yetYetAnotherClass } do: [ :each | 
		each classLayout checkIntegrity.
		self assert: each isVariable description: each name, ' was not reshaped.'.
		self assert: each isBytes description: each name, ' was not reshaped.' ]
]

{ #category : #tests }
SlotMigrationTest >> testReshapeSuperSuperClass [
	| supersuperclass superclass subclass |
	supersuperclass := self
		make: [ :builder | 
			builder
				superclass: Object;
				name: self aClassName;
				slots: #(a b) ].

	self assertEmpty: supersuperclass subclasses.
	supersuperclass classLayout checkIntegrity.

	superclass := self
		make: [ :builder | 
			builder
				superclass: supersuperclass;
				name: self anotherClassName ].

	self assert: supersuperclass subclasses equals: {superclass}.
	self assertEmpty: superclass subclasses.
	superclass classLayout checkIntegrity.
	supersuperclass classLayout checkIntegrity.

	subclass := self
		make: [ :builder | 
			builder
				superclass: superclass;
				name: self yetAnotherClassName;
				slots: #(c d) ].

	self assert: supersuperclass subclasses equals: {superclass}.
	self assert: superclass subclasses equals: {subclass}.
	self assertEmpty: subclass subclasses.
	subclass classLayout checkIntegrity.
	superclass classLayout checkIntegrity.
	supersuperclass classLayout checkIntegrity.

	"reshape the super super class"
	supersuperclass := self
		make: [ :builder | 
			builder
				superclass: Object;
				name: self aClassName;
				slots: #(a b a2) ].

	self assert: supersuperclass subclasses equals: {superclass}.
	self assert: superclass subclasses equals: {subclass}.
	self assertEmpty: subclass subclasses.
	subclass classLayout checkIntegrity.
	superclass classLayout checkIntegrity.
	supersuperclass classLayout checkIntegrity
]

{ #category : #tests }
SlotMigrationTest >> testShiftSlotAndMigrate [

	aClass := self makeWithLayout: FixedLayout andSlots: { #a. #c }.
	aClass compile: 'a ^a'.
	aClass compile: 'c ^c'.
	
	instance := aClass new.
	instance instVarNamed: #a put: 1.
	instance instVarNamed: #c put: 3.

	"now we extend it with slots.  The class definition should've changed"
	self makeWithLayout: FixedLayout andSlots: { #a. #b. #c }.

	"it should have added an instance variable with nil in the middle"
	self assert: (instance instVarAt: 1) equals: 1.
	self assert: (instance instVarAt: 2) equals: nil.
	self assert: (instance instVarAt: 3) equals: 3.
	
	self assert: (instance instVarNamed: #a) equals: 1.
	self assert: (instance instVarNamed: #b) equals: nil.
	self assert: (instance instVarNamed: #c) equals: 3.
	
	"it should have migrated methods"
	self assert: instance a equals: 1.
	self assert: instance c equals: 3.
	aClass classLayout checkIntegrity
]

{ #category : #tests }
SlotMigrationTest >> testSuperclassChangeLayoutType [
	"This case reproduces a MNU found loading Moose."

	| superclass class |
	superclass := self makeWithLayout: ByteLayout.
	class := self make: [ :builder |
		builder 
			superclass: superclass;
			layoutClass: ByteLayout;
			name: self anotherClassName.
		 ].
	"Change the layout of the superclass from bytes to pointer"
	self makeWithLayout: FixedLayout.

	superclass classLayout checkIntegrity.
	class classLayout checkIntegrity.
	
	self deny: superclass classLayout isBits.
	self assert: class classLayout isBits
]

{ #category : #tests }
SlotMigrationTest >> testSwitchSlotsAndMigrate [

	aClass := self makeWithLayout: FixedLayout andSlots: { #a. #b }.
	aClass 
		compile: 'a ^a';
		compile: 'b ^b'.

	instance := aClass new.
	instance instVarNamed: #a put: $A.
	instance instVarNamed: #b put: $B.

	"now we switch the slots. The class definition should've changed"
	self makeWithLayout: FixedLayout andSlots: { #b. #a }.

	self assert: (instance instVarAt: 1) equals: $B.
	self assert: (instance instVarAt: 2) equals: $A.
	
	self assert: (instance instVarNamed: #a) equals: $A.
	self assert: (instance instVarNamed: #b) equals: $B.

	"it should have migrated methods"
	self assert: instance a equals: $A.
	self assert: instance b equals: $B.
	aClass classLayout checkIntegrity
]
