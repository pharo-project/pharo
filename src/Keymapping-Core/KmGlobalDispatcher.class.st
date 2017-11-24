"
I am a dispatcher that dispatches the global keymappings.
"
Class {
	#name : #KmGlobalDispatcher,
	#superclass : #Object,
	#instVars : [
		'dispatcher'
	],
	#category : #'Keymapping-Core'
}

{ #category : #dispatching }
KmGlobalDispatcher >> dispatch: anEventBuffer [
	dispatcher
		dispatch: anEventBuffer
		inCategories: KMRepository default globalCategories

]

{ #category : #'initialize-release' }
KmGlobalDispatcher >> dispatcher: aDispatcher [
	dispatcher := aDispatcher
]

{ #category : #testing }
KmGlobalDispatcher >> isGlobalDispatcher [
	^true
]

{ #category : #iterating }
KmGlobalDispatcher >> nextForKmChain: aKMDispatchChain [ 
	^aKMDispatchChain target
]
