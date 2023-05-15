Class {
	#name : #FLPlatform,
	#superclass : #Object,
	#classVars : [
		'Current'
	],
	#category : #'Fuel-Platform-Core'
}

{ #category : #accessing }
FLPlatform class >> current [
	^ Current ifNil: [ Current := self detectPlatform ]
]

{ #category : #private }
FLPlatform class >> detectPlatform [
	| matches mostSpecific |
	matches := self allSubclasses 
		select: [ :class | class isResponsibleForCurrentPlatform ].
	mostSpecific := matches first.
	matches do: [ :class |
		class allSubclasses size < mostSpecific allSubclasses size ifTrue: [
			mostSpecific := class ] ].
	
	^ mostSpecific new
]

{ #category : #testing }
FLPlatform class >> isResponsibleForCurrentPlatform [
	^ self subclassResponsibility
]

{ #category : #'class initialization' }
FLPlatform class >> reset [
	Current := nil
]
