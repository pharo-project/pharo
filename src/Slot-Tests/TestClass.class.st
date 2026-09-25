Class {
	#name : 'TestClass',
	#superclass : 'TestRootClass',
	#category : 'Slot-Tests-Data',
	#package : 'Slot-Tests',
	#tag : 'Data'
}

{ #category : 'as yet unclassified' }
TestClass class >> classSideMethodUsingInheritedSomeShareVariable [
	^ SomeSharedVariable 
]

{ #category : 'as yet unclassified' }
TestClass >> anotherMethodUsingSomeSharedVariable [
	^ SomeSharedVariable + 1
]

{ #category : 'as yet unclassified' }
TestClass >> methodUsingInheritedSharedPoolVariable [
	^ One
]

{ #category : 'as yet unclassified' }
TestClass >> methodUsingSomeSharedVariable [
	^ SomeSharedVariable + 1
]
