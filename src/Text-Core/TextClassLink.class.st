"
An active link in a text linking to a class
"
Class {
	#name : #TextClassLink,
	#superclass : #TextLink,
	#instVars : [
		'className'
	],
	#category : #'Text-Core'
}

{ #category : #'instance creation' }
TextClassLink class >> class: aClass [
	^ self className: aClass name
]

{ #category : #'instance creation' }
TextClassLink class >> className: aSymbol [
	^ self new
		className: aSymbol;
		yourself
]

{ #category : #comparing }
TextClassLink >> = anotherObject [

	^super = anotherObject and: [ className = anotherObject className ]
]

{ #category : #'system primitives' }
TextClassLink >> className [	
	^ className
]

{ #category : #accessing }
TextClassLink >> className: aStringOrSymbol [
	className := aStringOrSymbol asSymbol
]

{ #category : #comparing }
TextClassLink >> hash [

	^(super hash + className hash) hashMultiply 
]
