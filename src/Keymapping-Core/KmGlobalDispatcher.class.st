"
I am a dispatcher that dispatches the global keymappings.
"
Class {
	#name : #KMGlobalDispatcher,
	#superclass : #Object,
	#instVars : [
		'dispatcher'
	],
	#category : #'Keymapping-Core-Dispatching'
}

{ #category : #dispatching }
KMGlobalDispatcher >> dispatch: anEventBuffer [
	dispatcher
		dispatch: anEventBuffer
		inCategories: KMRepository default globalCategories

]

{ #category : #initialization }
KMGlobalDispatcher >> dispatcher: aDispatcher [
	dispatcher := aDispatcher
]

{ #category : #testing }
KMGlobalDispatcher >> isGlobalDispatcher [
	^true
]

{ #category : #iterating }
KMGlobalDispatcher >> nextForKmChain: aKMDispatchChain [ 
	^aKMDispatchChain target
]
