Class {
	#name : #HEClass,
	#superclass : #HEMethodContainer,
	#instVars : [
		'className',
		'superclass',
		'instanceVariables',
		'classInstancevariables',
		'classVariables',
		'sharedPools',
		'layoutClass'
	],
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEClass >> classInstancevariables [
	^ classInstancevariables
]

{ #category : #accessing }
HEClass >> classInstancevariables: anObject [
	classInstancevariables := anObject
]

{ #category : #initialization }
HEClass >> className [
	^ className
]

{ #category : #accessing }
HEClass >> className: anObject [
	className := anObject
]

{ #category : #accessing }
HEClass >> classVariables [
	^ classVariables
]

{ #category : #accessing }
HEClass >> classVariables: anObject [
	classVariables := anObject
]

{ #category : #reading }
HEClass >> doReadHeaderFrom: aReader [
	className := aReader readByteSymbol.
	superclass := aReader readByteSymbol.
	instanceVariables := aReader readByteString.
	classInstancevariables := aReader readByteString.
	classVariables := aReader readByteString.
	sharedPools := aReader readByteString.
	layoutClass := aReader readByteSymbol
]

{ #category : #accessing }
HEClass >> instanceVariables [
	^ instanceVariables
]

{ #category : #accessing }
HEClass >> instanceVariables: anObject [
	instanceVariables := anObject
]

{ #category : #accessing }
HEClass >> isTrait [
	^ false
]

{ #category : #accessing }
HEClass >> layoutClass [
	^ layoutClass
]

{ #category : #accessing }
HEClass >> layoutClass: anObject [
	layoutClass := anObject
]

{ #category : #printing }
HEClass >> printOn: aStream [
	aStream
		nextPutAll: 'HEClass ( ';
		nextPutAll: className asString;
		nextPutAll: ' )'
]

{ #category : #accessing }
HEClass >> sharedPools [
	^ sharedPools
]

{ #category : #accessing }
HEClass >> sharedPools: anObject [
	sharedPools := anObject
]

{ #category : #accessing }
HEClass >> superclass [
	^ superclass ifEmpty: [nil]
]

{ #category : #accessing }
HEClass >> superclass: anObject [
	superclass := anObject
]
