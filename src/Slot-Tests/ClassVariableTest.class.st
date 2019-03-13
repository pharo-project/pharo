"
Tests for ClassVariable
"
Class {
	#name : #ClassVariableTest,
	#superclass : #TestCase,
	#classVars : [
		'TestVariable'
	],
	#category : #'Slot-Tests-VariablesAndSlots'
}

{ #category : #'tests  - properties' }
ClassVariableTest >> testIsReadInMethod [

	DefaultTimeLimit printString. "reading class variable".
	
	self assert: ((TestCase classVariableNamed: #DefaultTimeLimit) isReadIn: self class >> testSelector)
]

{ #category : #'tests  - properties' }
ClassVariableTest >> testIsWrittenInMethod [

	DefaultTimeLimit := DefaultTimeLimit. "writing class variable".
	
	self assert: ((TestCase classVariableNamed: #DefaultTimeLimit) isWrittenIn: self class >> testSelector)
]

{ #category : #'tests  - properties' }
ClassVariableTest >> testNotReadInMethod [

	self deny: ((TestCase classVariableNamed: #DefaultTimeLimit) isReadIn: self class >> testSelector)
]

{ #category : #'tests  - properties' }
ClassVariableTest >> testNotWrittenInMethodWhenItIsOnlyRead [

	DefaultTimeLimit printString. "reading class variable".
	
	self deny: ((TestCase classVariableNamed: #DefaultTimeLimit) isWrittenIn: self class >> testSelector)
]

{ #category : #'tests  - properties' }
ClassVariableTest >> testPropertyAtPut [

	| testValue classVariable |
	
	testValue := Date today.
	classVariable := self class classVariableNamed: #TestVariable.
	
	classVariable propertyAt: #testKeySelector put: testValue.
	self
		assert: (classVariable propertyAt: #testKeySelector)
		equals: testValue.

	TestVariable := 5. "of course changing the var does not change the property"
	
	self
		assert: (classVariable propertyAt: #testKeySelector)
		equals: testValue.
		
	classVariable removeProperty: #testKeySelector.
	self assert: classVariable properties isNil.
]

{ #category : #'tests  - properties' }
ClassVariableTest >> testRemoveProperty [
	| classVariable |
	classVariable := self class classVariableNamed: #TestVariable.

	classVariable propertyAt: #testKeySelector put: 1.
	classVariable removeProperty: #testKeySelector.
	self
		assert: (classVariable propertyAt: #testKeySelector)
		equals: nil.
	self assert: classVariable properties isNil.
]
