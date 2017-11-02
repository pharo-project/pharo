"
Just a Simple class used for tests
"
Class {
	#name : #HETeSimpleClass,
	#superclass : #Object,
	#instVars : [
		'anInstanceVariable'
	],
	#classVars : [
		'AClassVariable'
	],
	#classInstVars : [
		'aClassInstanceVariable'
	],
	#category : 'Hermes-Tests-Test-Classes'
}

{ #category : #'instance creation' }
HETeSimpleClass class >> newInstance [
	^ self new.
]

{ #category : #accessing }
HETeSimpleClass >> anInstanceVariable [
	^ anInstanceVariable
]

{ #category : #accessing }
HETeSimpleClass >> anInstanceVariable: anObject [
	anInstanceVariable := anObject
]

{ #category : #'as yet unclassified' }
HETeSimpleClass >> anUselessMethodWithPragma [
	<someUselessPragma: 1 withArgs: #(1 2 3) with: #HETeSimpleClass>
]

{ #category : #'as yet unclassified' }
HETeSimpleClass >> otherMethod [
	^ self anInstanceVariable 
]

{ #category : #'as yet unclassified' }
HETeSimpleClass >> pragmaMethod [
	<pragma>
]

{ #category : #'as yet unclassified' }
HETeSimpleClass >> usingAConstant [
	^ NumberParser parse:'42'.
]

{ #category : #'as yet unclassified' }
HETeSimpleClass >> usingMoreConstants [
	^ true printString , #() printString , #asd printString , 1.223 printString
		, #[1 2 3] printString, $A printString
]
