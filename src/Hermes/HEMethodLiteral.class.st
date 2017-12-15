Class {
	#name : #HEMethodLiteral,
	#superclass : #HEExportedLiteral,
	#instVars : [
		'className',
		'selector'
	],
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEMethodLiteral class >> tag [
	^ 10.
]

{ #category : #converting }
HEMethodLiteral >> asLiteralIn: env [
	^ (env newClass name = className and: [ env newSelector = selector ])
		ifTrue: [ env newMethod ]
		ifFalse: [ (env classNamed: className) >> selector ]
]

{ #category : #introspection }
HEMethodLiteral >> className [
	^ className
]

{ #category : #accessing }
HEMethodLiteral >> className: anObject [
	className := anObject
]

{ #category : #reading }
HEMethodLiteral >> doReadFrom: aReader [
	className := aReader readByteSymbol.
	selector := aReader readByteSymbol.


]

{ #category : #accessing }
HEMethodLiteral >> value: aMethod [
	className := aMethod methodClass name.
	selector := aMethod selector.
]
