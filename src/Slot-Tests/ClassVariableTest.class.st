"
Tests for ClassVariable
"
Class {
	#name : 'ClassVariableTest',
	#superclass : 'TestCase',
	#classVars : [
		'TestVariable'
	],
	#pools : [
		'TestSharedPool'
	],
	#category : 'Slot-Tests-VariablesAndSlots',
	#package : 'Slot-Tests',
	#tag : 'VariablesAndSlots'
}

{ #category : 'tests - properties' }
ClassVariableTest >> testIsReadInMethod [

	DefaultTimeLimit printString. "reading class variable".

	self assert: ((TestCase classVariableNamed: #DefaultTimeLimit) isReadIn: self class >> testSelector)
]

{ #category : 'tests' }
ClassVariableTest >> testIsReferenced [
	self assert: (SmalltalkImage classVariableNamed: #CompilerClass) isReferenced.
	TestVariable.
	self assert: (self class classVariableNamed: #TestVariable) isReferenced.
	"Check that it works for references to pool vars in the pool defining class"
	self assert: (TestSharedPool classVariableNamed: #One) isReferenced.
	ReferencedInTest.
	self assert: (TestSharedPool classVariableNamed: #ReferencedInTest) isReferenced
]

{ #category : 'tests' }
ClassVariableTest >> testIsShadowing [
	| variable |
	variable := SmalltalkImage classVariableNamed: #CompilerClass.
	self deny: variable isShadowing
]

{ #category : 'tests - properties' }
ClassVariableTest >> testIsWrittenInMethod [

	DefaultTimeLimit := DefaultTimeLimit. "writing class variable".

	self assert: ((TestCase classVariableNamed: #DefaultTimeLimit) isWrittenIn: self class >> testSelector)
]

{ #category : 'tests - properties' }
ClassVariableTest >> testNotReadInMethod [

	self deny: ((TestCase classVariableNamed: #DefaultTimeLimit) isReadIn: self class >> testSelector)
]

{ #category : 'tests - properties' }
ClassVariableTest >> testNotWrittenInMethodWhenItIsOnlyRead [

	| constantlockForTest |
	DefaultTimeLimit printString. "reading class variable".
	"add a clean block to check that is works"
	constantlockForTest := [  ].

	self deny: ((TestCase classVariableNamed: #DefaultTimeLimit) isWrittenIn: self class >> testSelector)
]

{ #category : 'tests' }
ClassVariableTest >> testPossiblyUsingClasses [
	| variable |

	"this is a normal class var"
	variable := CharacterScanner classVariableNamed: #CompositionStopConditions.

	"it can be accessed from the class and the meta class"
	self assert: (variable possiblyUsingClasses includes: CharacterScanner).
	self assert: (variable possiblyUsingClasses includes: CharacterScanner class).
	"and subclasses"
	self assert: (variable possiblyUsingClasses includes: CompositionScanner).
	self assert: (variable possiblyUsingClasses includes: CompositionScanner class).

	"Pools are more complex"
	variable := TextConstants classVariableNamed: #Basal.
	self assert: variable isPoolVariable.
	"same for pool var: class and meta class of the defining class"
	self assert: (variable possiblyUsingClasses includes: TextConstants).
	self assert: (variable possiblyUsingClasses includes: TextConstants class).
	"but in addition the all the users"
	self assert: (variable possiblyUsingClasses includes: CharacterScanner).
	self assert: (variable possiblyUsingClasses includes: CharacterScanner class).
	"and subclasses"
	self assert: (variable possiblyUsingClasses includes: CompositionScanner).
	self assert: (variable possiblyUsingClasses includes: CompositionScanner class)
]

{ #category : 'tests' }
ClassVariableTest >> testPrintOn [
	| variable |
	variable := SmalltalkImage classVariableNamed: #CompilerClass.
	self assert: variable definitionString equals: '#CompilerClass'.
	"but we print as a self-evaluation varibale defition"
	self assert: variable printString equals: '#CompilerClass => ClassVariable'
]

{ #category : 'tests - properties' }
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
	self assert: classVariable properties isNil
]

{ #category : 'tests - properties' }
ClassVariableTest >> testReadingFromContext [

	| classVariable |
	TestVariable := #testValue.
	classVariable := self class classVariableNamed: #TestVariable.

	self
		assert: (classVariable readInContext: thisContext)
		equals: #testValue
]

{ #category : 'tests - properties' }
ClassVariableTest >> testRemoveProperty [
	| classVariable |
	classVariable := self class classVariableNamed: #TestVariable.

	classVariable propertyAt: #testKeySelector put: 1.
	classVariable removeProperty: #testKeySelector.
	self
		assert: (classVariable propertyAt: #testKeySelector)
		equals: nil.
	self assert: classVariable properties isNil
]

{ #category : 'tests' }
ClassVariableTest >> testScope [
	| variable |
	variable := SmalltalkImage classVariableNamed: #CompilerClass.
	self assert: variable scope equals: SmalltalkImage.
	self assert: (variable scope lookupVar: variable name) equals: variable
]

{ #category : 'tests' }
ClassVariableTest >> testUsingMethods [
	| variable |
	"The semantics of where to search is tested as part of #testPossiblyUsingClasses"

	variable := CharacterScanner classVariableNamed: #CompositionStopConditions.
	self assert: (variable usingMethods includes: CompositionScanner>>#setStopConditions).

	variable := TextConstants classVariableNamed: #DefaultMarginTabsArray.
	self assert: variable isPoolVariable.
	self assert: (variable usingMethods includes: TextConstants class>>#DefaultMarginTabsArray).
	self assert: (variable usingMethods includes: TextStyle>>#newFontArray:)
]

{ #category : 'tests - properties' }
ClassVariableTest >> testWritingToContext [

	| classVariable |
	classVariable := self class classVariableNamed: #TestVariable.
	classVariable write: #testValue inContext: thisContext.

	self assert: TestVariable equals: #testValue
]

{ #category : 'tests' }
ClassVariableTest >> testisPoolVariable [
	| variable |
	variable := SmalltalkImage classVariableNamed: #CompilerClass.
	self deny: variable isPoolVariable.
	variable := TextConstants classVariableNamed: #Basal.
	self assert: variable isPoolVariable
]
