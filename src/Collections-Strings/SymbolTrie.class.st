Class {
	#name : #SymbolTrie,
	#superclass : #Object,
	#instVars : [
		'roots'
	],
	#category : #'Collections-Strings-Base'
}

{ #category : #accessing }
SymbolTrie >> roots [

	^ roots
]

{ #category : #accessing }
SymbolTrie >> roots: anObject [

	roots := anObject
]
