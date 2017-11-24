"
I know how to build ruler links.
"
Class {
	#name : #HiLinkBuilder,
	#superclass : #HiBuilder,
	#instVars : [
		'label',
		'color',
		'width',
		'rendererClass',
		'dashes',
		'conditionBlock',
		'targetsBlock'
	],
	#category : #'Hiedra-Builders'
}

{ #category : #accessing }
HiLinkBuilder >> color [
	
	^ color
]

{ #category : #accessing }
HiLinkBuilder >> color: anObject [
	
	color := anObject
]

{ #category : #accessing }
HiLinkBuilder >> conditionBlock [

	^ conditionBlock ifNil: [ 
		conditionBlock := [ :node | true ] ]
]

{ #category : #accessing }
HiLinkBuilder >> conditionBlock: aBlockClosure [

	conditionBlock := aBlockClosure
]

{ #category : #accessing }
HiLinkBuilder >> dashes [
	^ dashes
]

{ #category : #accessing }
HiLinkBuilder >> dashes: anArrayOfNumbers [
	dashes := anArrayOfNumbers
]

{ #category : #accessing }
HiLinkBuilder >> destinationsBlock: aBlockClosure [ 
	self 
		deprecated: 'Use targetsBlock: instead'
		on: '30 March 2016' 
		in: 'Pharo-5.0-50571-'.
	^ self targetsBlock: aBlockClosure
]

{ #category : #initialization }
HiLinkBuilder >> initialize [
	super initialize.
	
	rendererClass := HiSnakeLinkRenderer.
	dashes := #().
	width := 2.1.
	color := Smalltalk ui theme caretColor.

]

{ #category : #accessing }
HiLinkBuilder >> label [
	^ label
]

{ #category : #accessing }
HiLinkBuilder >> label: anObject [
	label := anObject
]

{ #category : #printing }
HiLinkBuilder >> printOn: aStream [
	"FIX: for debugging"
	label ifNil: [ ^ super printOn: aStream ].
	
	label printOn: aStream.
]

{ #category : #building }
HiLinkBuilder >> renderer [
	^ self rendererClass new
		linkBuilder: self;
		yourself
]

{ #category : #accessing }
HiLinkBuilder >> rendererClass [
	^ rendererClass
]

{ #category : #accessing }
HiLinkBuilder >> rendererClass: anObject [
	rendererClass := anObject
]

{ #category : #accessing }
HiLinkBuilder >> targetsBlock [
	^ targetsBlock
]

{ #category : #accessing }
HiLinkBuilder >> targetsBlock: aBlockClosure [
	targetsBlock := aBlockClosure
]

{ #category : #accessing }
HiLinkBuilder >> targetsOf: anObject [

	^ (self conditionBlock cull: anObject) 
		ifTrue: [ self targetsBlock cull: anObject ]
		ifFalse: [ #() ]
]

{ #category : #accessing }
HiLinkBuilder >> width [
	
	^ width
]

{ #category : #accessing }
HiLinkBuilder >> width: anObject [
	
	width := anObject
]
