"
my role is to record the movements of a finger and associate a color to this movement
"
Class {
	#name : #SDL2TouchStateMachine,
	#superclass : #Object,
	#instVars : [
		'first',
		'finish',
		'move',
		'color'
	],
	#category : 'OSWindow-SDL2-Examples-Touch'
}

{ #category : #'instance creation' }
SDL2TouchStateMachine class >> for:	anPosition [
	^self new 
		first: anPosition;
		yourself
]

{ #category : #accessing }
SDL2TouchStateMachine >> add: aPoint [
	move add: aPoint.
	
]

{ #category : #accessing }
SDL2TouchStateMachine >> color [
	^color
]

{ #category : #accessing }
SDL2TouchStateMachine >> finish [
	^ finish
]

{ #category : #accessing }
SDL2TouchStateMachine >> finish: anObject [
	finish := anObject.
	move add: anObject.
]

{ #category : #accessing }
SDL2TouchStateMachine >> first [
	^ first
]

{ #category : #accessing }
SDL2TouchStateMachine >> first: anObject [
	first := anObject
]

{ #category : #initialization }
SDL2TouchStateMachine >> initialize [
	move := OrderedCollection new.
	color := Color random.
]

{ #category : #accessing }
SDL2TouchStateMachine >> move [
	^ move
]

{ #category : #accessing }
SDL2TouchStateMachine >> move: anObject [
	move := anObject
]
