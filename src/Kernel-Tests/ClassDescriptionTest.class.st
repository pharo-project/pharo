"
This is the unit test for the class ClassDescription. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org)
	- the sunit class category
"
Class {
	#name : #ClassDescriptionTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Classes'
}

{ #category : #tests }
ClassDescriptionTest >> testAllMethodCategoriesIntegratedThrough [
	"If this test fails, it may be because method protocols are sorted in #allMethodCategoriesIntegratedThrough. Take care that if the protocols of the class under test are already sorted, you won't see any problem."
	self assert: ((CompiledMethod selectorsInProtocol: 'testing') includes: #isAbstract).
	CompiledMethod allProtocolsUpTo: Object.
	self assert: ((CompiledMethod selectorsInProtocol: 'testing') includes: #isAbstract)
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testAllSlots [
	self assert: Context allSlots size equals: 6
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testDefinesSlotNamed [
	self assert: (Point definesSlotNamed: #x).
	self deny: (Point definesSlotNamed: #z).
	
	self assert: (Context definesSlotNamed: #stackp).
	self deny: (Context definesSlotNamed: #sender).
	
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testHasSlot [
	self assert: (Context hasSlot: (Context slotNamed: #sender)).
	self deny: (Context definesSlot: (Context slotNamed: #sender)).
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testHasSlotNamed [
	self assert: (Context hasSlotNamed: #sender).
	self assert: (Context hasSlotNamed: #method).
]

{ #category : #tests }
ClassDescriptionTest >> testMethods [
	self assert: Object methods equals: Object methodDict values
]

{ #category : #tests }
ClassDescriptionTest >> testNumberOfMethods [
	self assert: Point numberOfMethods equals: Point localMethods size + Point class localMethods size
]

{ #category : #tests }
ClassDescriptionTest >> testOrganization [

	| aClassOrganizer |
	aClassOrganizer := ClassDescription organization.
	self assert: (aClassOrganizer isKindOf: ClassOrganization).
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testSlotNamed [
	self assert: (Point slotNamed: #x) name equals: #x
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testSlotNames [
	self assert: Point slotNames equals: #(x y)
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testSlots [
	self assert: Context slots size equals: 4
]

{ #category : #'tests - instance variables' }
ClassDescriptionTest >> testclassThatDefinesInstVarNamed [
	self assert: (Point classThatDefinesInstVarNamed: 'x') equals: Point.
	self assert: (Class classThatDefinesInstVarNamed: 'methodDict') equals: Behavior.
	self assert: (Point classThatDefinesInstVarNamed: 'methodDict') equals: nil.
	
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testprintHierarchy [
	| expected result |
	expected := '
ProtoObject #()
	Object #()

		ExampleForTest1 #()
			ExampleForTest11 #()
				ExampleForTest111 #()
				ExampleForTest112 #()
			ExampleForTest12 #()'.
	result := ExampleForTest1 printHierarchy.
	self assert: result equals: expected
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testprintSubclassesOnLevelFilterA [
	| expected result stream |
	expected := '
	ExampleForTest1 #()
		ExampleForTest11 #()
			ExampleForTest112 #()'.
	result := String new: expected size.
	stream := ReadWriteStream on: result.
	ExampleForTest1 printSubclassesOn: stream level: 1 filter: {ExampleForTest1. ExampleForTest11. ExampleForTest112}.
	self assert: result equals: expected
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testprintSubclassesOnLevelFilterB [
	| expected result stream |
	expected := '
	ExampleForTest1 #()
		ExampleForTest12 #()'.
	result := String new: expected size.
	stream := ReadWriteStream on: result.
	ExampleForTest1 printSubclassesOn: stream level: 1 filter: { ExampleForTest1. ExampleForTest12. ExampleForTest112 }.
	self assert: result equals: expected
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testprintSubclassesOnLevelFilterNil [
	| expected result stream |
	expected := '
	ExampleForTest1 #()
		ExampleForTest11 #()
			ExampleForTest111 #()
			ExampleForTest112 #()
		ExampleForTest12 #()'.
	result := String new: expected size.
	stream := ReadWriteStream on: result.
	ExampleForTest1 printSubclassesOn: stream level: 1 filter: nil.
	self assert: result equals: expected
]
