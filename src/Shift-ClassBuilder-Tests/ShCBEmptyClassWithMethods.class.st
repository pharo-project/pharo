"
I am a test class.
"
Class {
	#name : #ShCBEmptyClassWithMethods,
	#superclass : #Object,
	#category : 'Shift-ClassBuilder-Tests-TestClasses'
}

{ #category : #'test methods' }
ShCBEmptyClassWithMethods >> aMethod [
	^ 42
]

{ #category : #'test methods' }
ShCBEmptyClassWithMethods >> anotherMethod: aValue [
	^ aValue.
]
