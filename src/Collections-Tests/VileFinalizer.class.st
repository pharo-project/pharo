"
I am test object used for a WeakRegistry test.
I add myself to a given WeakRegistry and on finalization I modify the very same registry by removing myself from it.
"
Class {
	#name : #VileFinalizer,
	#superclass : #Object,
	#instVars : [
		'weakRegistry'
	],
	#category : #'Collections-Tests-Weak'
}

{ #category : #finalization }
VileFinalizer >> finalize [
	weakRegistry remove: self ifAbsent: [].
]

{ #category : #initialization }
VileFinalizer >> initialize: registry [
	weakRegistry := registry.
	weakRegistry add: self.
]
