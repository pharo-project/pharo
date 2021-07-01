Class {
	#name : #SlotErrorsTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-ClassBuilder'
}

{ #category : #helpers }
SlotErrorsTest >> assertInvalidClassName: aName [

	self should: [
		self make: [ :builder | 
			builder name: aName ] ]
		raise: InvalidGlobalName.
	self flag: 'should remove class if not raised'.
		
	self should: [
		Smalltalk classInstaller 
			validateClassName: aName ]
		raise: InvalidGlobalName
]

{ #category : #tests }
SlotErrorsTest >> testCannotBeRecompiled [
	| superclass |
	<expectedFailure>
	superclass := self make: [ :builder | 
		builder 
			superclass: Object;
			name: self aClassName ].

	self make: [ :builder | 
		builder 
			superclass: superclass;
			name: self anotherClassName ].

	self 
		should: [ 
			self make: [ :builder | 
				builder 
					layoutClass: VariableLayout;
					name: self aClassName ] ]
		raise: Error
		description: 'Old class builder raises: X cannot be recompiled'

]

{ #category : #tests }
SlotErrorsTest >> testClassNameMustBeCapitalized [

	self assertInvalidClassName: #aNotCapitalizedClassName
]

{ #category : #tests }
SlotErrorsTest >> testClassNameMustBeSymbol [

	self assertInvalidClassName: 1
]

{ #category : #tests }
SlotErrorsTest >> testClassNameWithInvalidCharacter [

	self assertInvalidClassName: #'Invalid-ClassName'
]

{ #category : #tests }
SlotErrorsTest >> testClassSlotDuplicationConflict [

	aClass := self make: [ :builder | 
		builder classSlots: { #a } ].

	self should: [ 
		self make: [ :builder | 
			builder 
				superclass: aClass;
				name: self anotherClassName;
				classSlots: { #a } ] ]
		raise: DuplicatedSlotName 
]

{ #category : #tests }
SlotErrorsTest >> testClassSlotWithReservedName [

	#(#self #super #thisContext #true #false #nil) do: [:reservedName |
		self should: [ 
			self make: [ :builder | 
				builder classSlots: { reservedName } ] ]
			raise: InvalidSlotName ]
]

{ #category : #tests }
SlotErrorsTest >> testDangerousClassesConditions [

	| specialObjectsArrayItem |
	
	specialObjectsArrayItem := (Smalltalk specialObjectsArray select: [ :x | x isClass ]) anyOne name.

	self assert: (DangerousClassNotifier shouldNotBeRedefined: #ProtoObject).
	self assert: (DangerousClassNotifier shouldNotBeRedefined: specialObjectsArrayItem)
]

{ #category : #tests }
SlotErrorsTest >> testDangerousClassesEnabling [

	| savedExistingSystemSetting myExpectedError |

	savedExistingSystemSetting := DangerousClassNotifier enabled.

	DangerousClassNotifier disable.
	self assert: DangerousClassNotifier enabled not.
	DangerousClassNotifier disableDuring: [
      self assert: DangerousClassNotifier enabled not].
	self assert: DangerousClassNotifier enabled not.

	DangerousClassNotifier enable.
	self assert: DangerousClassNotifier enabled.	
	DangerousClassNotifier disableDuring: [
      self assert: DangerousClassNotifier enabled not].
	self assert: DangerousClassNotifier enabled.

	DangerousClassNotifier enable.
	DangerousClassNotifier disableDuring: [
		self deny: (DangerousClassNotifier check: #CompiledMethod)].
	myExpectedError := false.
	[ DangerousClassNotifier check: #CompiledMethod ] on: Warning do: [ myExpectedError := true  ].
	self assert: myExpectedError.

	savedExistingSystemSetting 
		ifTrue: [ DangerousClassNotifier enable ]
		ifFalse: [ DangerousClassNotifier disable ].
	self assert: DangerousClassNotifier enabled equals: savedExistingSystemSetting
]

{ #category : #tests }
SlotErrorsTest >> testDirectCircularHierarchyError [
	"Tests an error is raised when trying to create a hierarchy A<-A"
	
	| classA |
	classA := self make: [ :builder |
			builder 
				superclass: Object;
				name: self aClassName ].

	self should: [ 
		self make: [ :builder | 
			builder 
				superclass: classA;
				name: self aClassName ] ]
		raise: CircularHierarchyError 
]

{ #category : #tests }
SlotErrorsTest >> testIndirectCircularHierarchyError [
	"Tests an error is raised when trying to create a hierarchy A<-B<-A"
	
	| classA classB |
	classA := self make: [ :builder |
			builder 
				superclass: Object;
				name: self aClassName ].

	classB := self make: [ :builder |
			builder 
				superclass: classA;
				name: self anotherClassName ].

	self should: [ 
		self make: [ :builder | 
			builder 
				superclass: classB;
				name: self aClassName ] ]
		raise: CircularHierarchyError 
]

{ #category : #tests }
SlotErrorsTest >> testSlotDuplicationConflict [

	aClass := self make: [ :builder | 
		builder slots: { #a } ].

	self should: [ 
		self make: [ :builder | 
			builder 
				superclass: aClass;
				name: self anotherClassName;
				slots: { #a } ] ]
		raise: DuplicatedSlotName 
]

{ #category : #tests }
SlotErrorsTest >> testSlotWithReservedName [

	#(#self #super #thisContext #true #false #nil) do: [:reservedName |
		self should: [ 
			self make: [ :builder | 
				builder slots: { reservedName } ] ]
			raise: InvalidSlotName ]
]

{ #category : #tests }
SlotErrorsTest >> testUndeclareSlot [
	| slot instance |

	aClass := self make: [ :builder | 
		builder slots: { UndefinedSlot named: #a ast: ((RBParser parseExpression: '#a => MySlot') doSemanticAnalysis;yourself) } ].

	slot := aClass slotNamed: #a.

	self assert: slot class equals: UndefinedSlot.
	self assert: slot name equals: #a.
	self assert: slot slotClassName equals: #MySlot.
	
	instance := aClass new.
	
	self assert: (slot read: instance) equals: nil.
	slot write: 1 to: instance.
	self assert: (slot read: instance) equals: nil.

]

{ #category : #tests }
SlotErrorsTest >> testUndeclareSlotFixWhenSlotIsLoaded [
	| slot instance mySlot |

	aClass := self make: [ :builder | 
		builder slots: { UndefinedSlot named: #a ast: ((RBParser parseExpression: '#a => ', self anotherClassName) doSemanticAnalysis;yourself) } ].

	slot := aClass slotNamed: #a.

	self assert: slot class equals: UndefinedSlot.
	self assert: slot name equals: #a.
	self assert: slot slotClassName equals: self anotherClassName.
	
	instance := aClass new.
	
	self assert: (slot read: instance) equals: nil.
	slot write: 1 to: instance.
	self assert: (slot read: instance) equals: nil.

	self class classInstaller make: [ :aClassBuilder | 
		aClassBuilder 
			name: self anotherClassName;
			superclass: PropertySlot;
			package: self aCategory ].
	
	self assert: (slot read: instance) equals: nil.
	
	mySlot := aClass slotNamed: #a.
	
	self assert: mySlot name equals: #a.
	
	self assert: (mySlot read: instance) equals: nil.
	mySlot write: 1 to: instance.
	self assert: (mySlot read: instance) equals: 1.
]

{ #category : #tests }
SlotErrorsTest >> testValidateClassName [		
		
	Smalltalk classInstaller 
		validateClassName: #GoodClassNameThatShouldNotExist
]
