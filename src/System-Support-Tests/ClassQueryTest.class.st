"
Unit tests for queries on classes
"
Class {
	#name : #ClassQueryTest,
	#superclass : #TestCase,
	#category : #'System-Support-Tests'
}

{ #category : #tests }
ClassQueryTest >> testAllCallsOn [
	| calls |
	calls := ClassQueryTest allCallsOn.
	self assert: calls size equals: 1.
	self assert: calls first compiledMethod equals: ClassQueryTest>>#testAllCallsOn
]

{ #category : #tests }
ClassQueryTest >> testAllCallsOnASymbol [

	| set cm |
	set := Object allCallsOn: #shallowCopy.
	cm := (set detect: [ :rgMethod | (rgMethod selector == #copy) and: [rgMethod methodClass = Object]]).
	self assert: (cm literals includes: #shallowCopy)
]

{ #category : #tests }
ClassQueryTest >> testReferencedClasses [
	
	| refs |
	refs := Metaclass referencedClasses.
	self assert: (refs includes: Slot).
	
	refs := self class referencedClasses.
	self assert: (refs includesAll: { Slot. Metaclass })
]
