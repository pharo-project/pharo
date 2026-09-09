"
I am an object that controls the dispatch order of keymaps given a hierarchy of morphs.

"
Class {
	#name : 'KMDispatchChain',
	#superclass : 'Object',
	#instVars : [
		'target',
		'dispatcher',
		'initialTarget'
	],
	#category : 'Keymapping-Core-Dispatching',
	#package : 'Keymapping-Core',
	#tag : 'Dispatching'
}

{ #category : 'instance creation' }
KMDispatchChain class >> from: anInitialTarget andDispatcher: aDispatcher [
	^self new
			dispatcher: aDispatcher;
			startOn: anInitialTarget;
			yourself
]

{ #category : 'dispatching' }
KMDispatchChain >> dispatch: aKeyboardEvent [
	self do: [ :targetToDispatch |
		| sequence |
		sequence := KMBuffer uniqueInstance buffer copy.
		targetToDispatch dispatch: sequence.
		aKeyboardEvent wasHandled ifTrue: [ ^self ].
		"Let's try to match this sequence of events again.
		This time ignoring any repeated events."
		sequence removeAllSuchThat: [ :ev | ev isRepeat ].
		sequence isNotEmpty ifTrue: [
			targetToDispatch dispatch: sequence.
			aKeyboardEvent wasHandled ifTrue: [ ^self ] ]
	].
	"This should be a noMatch event"
	aKeyboardEvent wasHandled ifFalse: [ KMBuffer uniqueInstance clearBuffer ]
]

{ #category : 'initialization' }
KMDispatchChain >> dispatcher: aDispatcher [
	dispatcher := aDispatcher.
	self target: dispatcher target
]

{ #category : 'iterating' }
KMDispatchChain >> do: aBlock [
	| currentTarget |
	currentTarget := initialTarget.
	[ currentTarget isNil ]
		whileFalse: [
			aBlock value: currentTarget.
			currentTarget := currentTarget nextForKmChain: self.
		]
]

{ #category : 'initialization' }
KMDispatchChain >> startOn: anInitialTarget [
	initialTarget := anInitialTarget
]

{ #category : 'iterating' }
KMDispatchChain >> target [
	^target
]

{ #category : 'initialization' }
KMDispatchChain >> target: aTarget [
	target := aTarget
]
