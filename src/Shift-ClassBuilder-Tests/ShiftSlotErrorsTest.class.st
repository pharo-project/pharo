Class {
	#name : 'ShiftSlotErrorsTest',
	#superclass : 'SlotErrorsTest',
	#category : 'Shift-ClassBuilder-Tests-Base',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Base'
}

{ #category : 'helpers' }
ShiftSlotErrorsTest >> assertInvalidClassName: aName [

	self should: [
		self make: [ :builder |
			builder name: aName ] ]
		raise: InvalidGlobalName
]

{ #category : 'tests' }
ShiftSlotErrorsTest >> testClassNameMustBeCapitalized [

	self assertInvalidClassName: #aNotCapitalizedClassName
]

{ #category : 'tests' }
ShiftSlotErrorsTest >> testClassNameMustBeSymbol [

	self assertInvalidClassName: 1
]

{ #category : 'tests' }
ShiftSlotErrorsTest >> testClassNameWithInvalidCharacter [

	self assertInvalidClassName: #'Invalid-ClassName'
]

{ #category : 'tests' }
ShiftSlotErrorsTest >> testDirectCircularHierarchyError [
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

{ #category : 'tests' }
ShiftSlotErrorsTest >> testIndirectCircularHierarchyError [
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
