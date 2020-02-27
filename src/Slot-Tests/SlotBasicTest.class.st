Class {
	#name : #SlotBasicTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-ClassBuilder'
}

{ #category : #'tests-class slots' }
SlotBasicTest >> testAddClassSlotAndMigrate [

	| classAdded classModified |
	classAdded := 
		self make: [ :builder | 
			builder classSlots: #(a) ].

	classAdded instVarNamed: #a put: $A.
	
	classModified := 
		self make: [ :builder | 
			builder classSlots: #(a b) ].
	
	self assert: (classModified instVarNamed: #a) equals: $A.
	self assert: (classModified instVarNamed: #b) isNil
]

{ #category : #'tests-shared pools' }
SlotBasicTest >> testAddSharedPool [

	self make: [ :builder | 
		builder sharedPools: '' ].

	aClass := self make: [ :builder | 
		builder sharedPools: 'TestSharedPool' ].
	
	self assert: (aClass sharedPools includes: TestSharedPool)

]

{ #category : #'tests-shared variables' }
SlotBasicTest >> testAddSharedVariable [

	self make: [ :builder | 
		builder sharedVariablesFromString: '' ].

	aClass := self make: [ :builder | 
		builder sharedVariablesFromString: 'Var' ].
	
	self assert: aClass classVarNames equals: #(Var)

]

{ #category : #'tests-basic' }
SlotBasicTest >> testBasicClassBuilding [
	aClass := self make: [ :builder | builder name: self aClassName ].

	self deny: aClass classLayout isNil.
	self assert: aClass name equals: self aClassName.
	self assert: (self class environment at: self aClassName) identicalTo: aClass
]

{ #category : #'tests-comments' }
SlotBasicTest >> testClassWithComment [

	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			comment: 'A class Comment' ].
	
	self assert: aClass name equals: self aClassName.
	self assert: aClass comment equals: 'A class Comment'
	

]

{ #category : #'tests-comments' }
SlotBasicTest >> testClassWithCommentAndStamp [

	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			comment: 'A class Comment' stamp: (Object>>#halt) stamp ].
	
	self assert: aClass name equals: self aClassName.
	self assert: aClass comment equals: 'A class Comment'.
	self assert: aClass organization commentStamp equals: (Object>>#halt) stamp
	

]

{ #category : #'tests-basic' }
SlotBasicTest >> testNewCompiledMethodClass [

	aClass := self makeWithLayout: CompiledMethodLayout.
	self deny: aClass isPointers.
	self deny: aClass isFixed.
	self assert: aClass isBytes.
	self deny: aClass isWeak
]

{ #category : #'tests-basic' }
SlotBasicTest >> testNewPointerClass [
	aClass := self makeWithLayout: FixedLayout.
	self assert: aClass isPointers.
	self assert: aClass isFixed.
	self assertEmpty: aClass instVarNames
]

{ #category : #'tests-basic' }
SlotBasicTest >> testNewPointerClassWithSlots [

	aClass := self 
		makeWithLayout: FixedLayout
		andSlots: { #id. #name }.
	
	self assert: (aClass instVarNames includes: 'id').
	self assert: (aClass instVarNames includes: 'name')
]

{ #category : #'tests-basic' }
SlotBasicTest >> testNewVariableByteClass [

	aClass := self makeWithLayout: ByteLayout.
	self deny: aClass isPointers.
	self deny: aClass isFixed.
	self assert: aClass isBytes.
	self assert: aClass isVariable
]

{ #category : #'tests-basic' }
SlotBasicTest >> testNewVariableWordClass [

	aClass := self makeWithLayout: WordLayout.
	self deny: aClass isPointers.
	self deny: aClass isFixed.
	self assert: aClass isWords.
	self assert: aClass isVariable
]

{ #category : #'tests-basic' }
SlotBasicTest >> testNewWeakClass [

	aClass := self makeWithLayout: WeakLayout.
	self assert: aClass isPointers.
	self deny: aClass isFixed.
	self assert: aClass isWords.
	self assert: aClass isWeak
]

{ #category : #'tests-class slots' }
SlotBasicTest >> testRemoveClassSlotAndMigrate [

	| classAdded classModified |
	classAdded := 
		self make: [ :builder | 
			builder classSlots: #(a b) ].

	classAdded instVarNamed: #a put: $A.
	
	classModified := 
		self make: [ :builder | 
			builder classSlots: #(a) ].
	
	self assert: (classModified instVarNamed: #a) equals: $A.
	self should: [ classModified instVarNamed: #b ] raise: Error

]

{ #category : #'tests-shared pools' }
SlotBasicTest >> testRemoveSharedPool [
	self make: [ :builder | builder sharedPools: 'TestSharedPool' ].

	aClass := self make: [ :builder | builder sharedPools: '' ].

	self assertEmpty: aClass sharedPools
]

{ #category : #'tests-shared variables' }
SlotBasicTest >> testRemoveSharedVariable [
	self make: [ :builder | builder sharedVariablesFromString: 'Var' ].

	aClass := self make: [ :builder | builder sharedVariablesFromString: '' ].

	self assertEmpty: aClass classVarNames
]

{ #category : #'tests-class slots' }
SlotBasicTest >> testSwitchClassSlotAndMigrate [

	| classAdded classModified |	
	classAdded := 
		self make: [ :builder | 
			builder classSlots: #(a b) ].

	"Add methods"	
	classAdded class 
		compile: 'a ^a';
		compile: 'b ^b'.

	"Put values"
	classAdded instVarNamed: #a put: $A.
	classAdded instVarNamed: #b put: $B.

	"Modify the class"	
	classModified := 
		self make: [ :builder | 
			builder classSlots: #(b a) ].
	
	"The instance was migrated"
	self assert: (classModified instVarNamed: #a) equals: $A.
	self assert: (classModified instVarNamed: #b) equals: $B.
	
	"The methods were migrated"
	self assert: classModified a equals: $A.
	self assert: classModified b equals: $B

]

{ #category : #'tests-class slots' }
SlotBasicTest >> testWithClassSlots [

	aClass := self make: [ :builder | 
		builder classSlots: #(a) ].
	
	self assert: aClass class instVarNames equals: #(a).	
	aClass instVarNamed: #a put: $A.
	self assert: (aClass instVarNamed: #a) equals: $A
]

{ #category : #'tests-shared pools' }
SlotBasicTest >> testWithSharedPool [

	aClass := self make: [ :builder | 
		builder sharedPools: 'TestSharedPool' ].
	aClass class compile: 'one ^One'.
	
	self assert: (aClass sharedPools includes: TestSharedPool).
	self assert: aClass one equals: 1
]

{ #category : #'tests-shared variables' }
SlotBasicTest >> testWithSharedVariable [

	aClass := self make: [ :builder | 
		builder sharedVariablesFromString: 'Var' ].
	aClass class 
		compile: 'var:x  Var:=x';
		compile: 'var  ^Var'.
	
	aClass var: true.
	
	self assert: aClass classVarNames equals: #(Var).
	self assert: aClass var
]

{ #category : #'tests-class slots' }
SlotBasicTest >> testWithoutClassSlots [
	aClass := self make: [ :builder | builder classSlots: #() ].

	self assertEmpty: aClass classVarNames
]

{ #category : #'tests-shared pools' }
SlotBasicTest >> testWithoutSharedPools [
	aClass := self make: [ :builder | builder sharedPools: '' ].

	self assertEmpty: aClass sharedPools
]

{ #category : #'tests-shared variables' }
SlotBasicTest >> testWithoutSharedVariables [
	aClass := self make: [ :builder | builder sharedVariablesFromString: '' ].

	self assertEmpty: aClass classVarNames
]
