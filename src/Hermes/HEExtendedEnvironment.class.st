"
I wrap the target environment.
I provide access to all the elements in the environment and also to:

- The class being deserialized
- The method being deserialized
- The selector of the method being deserialized.
"
Class {
	#name : #HEExtendedEnvironment,
	#superclass : #Object,
	#instVars : [
		'inner',
		'newMethod',
		'newClass',
		'newSelector'
	],
	#category : #'Hermes-ClassInstaller'
}

{ #category : #accessing }
HEExtendedEnvironment >> classNamed: aClassName [
	^ newClass name = aClassName
		ifTrue: [ newClass ]
		ifFalse: [ inner classNamed: aClassName ]
]

{ #category : #accessing }
HEExtendedEnvironment >> inner [
	^ inner
]

{ #category : #accessing }
HEExtendedEnvironment >> inner: anObject [
	inner := anObject
]

{ #category : #accessing }
HEExtendedEnvironment >> newClass [
	^ newClass
]

{ #category : #accessing }
HEExtendedEnvironment >> newClass: anObject [
	newClass := anObject
]

{ #category : #accessing }
HEExtendedEnvironment >> newMethod [
	^ newMethod
]

{ #category : #accessing }
HEExtendedEnvironment >> newMethod: anObject [
	newMethod := anObject
]

{ #category : #accessing }
HEExtendedEnvironment >> newSelector [
	^ newSelector
]

{ #category : #accessing }
HEExtendedEnvironment >> newSelector: anObject [
	newSelector := anObject
]
