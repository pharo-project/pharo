"
SUnit tests for testing the dispatch chain
"
Class {
	#name : #KMDispatchChainTest,
	#superclass : #AbstractKeymappingTest,
	#category : #'Keymapping-Tests'
}

{ #category : #tests }
KMDispatchChainTest >> testGlobalIsFirst [
	| chain first |
	first := nil.
	chain := KMDispatchChain from: (KMGlobalDispatcher new) andDispatcher: (KMDispatcher new).
	chain do: [ :target | first ifNil: [ first := target ] ].
	self assert: first isGlobalDispatcher
]

{ #category : #tests }
KMDispatchChainTest >> testPassesOnTheTarget [
	| chain dispatcher passed |
	dispatcher := KMDispatcher target: Morph new.
	passed := false.
	chain := KMDispatchChain from: (KMGlobalDispatcher new) andDispatcher: dispatcher.
	chain do: [ :t | t == dispatcher target ifTrue: [ passed := true ] ].
	self assert: passed
]

{ #category : #tests }
KMDispatchChainTest >> testTargetDoesNotIterateTheNil [
	| chain |
	chain := KMDispatchChain from: (KMGlobalDispatcher new) andDispatcher: (KMDispatcher new).
	chain do: [ :target | self assert: target notNil ]
]
