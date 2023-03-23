Class {
	#name : #SymbolTrieNode,
	#superclass : #IdentityDictionary,
	#instVars : [
		'children',
		'symbol',
		'parent'
	],
	#category : #'Collections-Strings-Base'
}

{ #category : #accessing }
SymbolTrieNode >> children [

	^ children
]

{ #category : #accessing }
SymbolTrieNode >> children: anObject [

	children := anObject
]

{ #category : #accessing }
SymbolTrieNode >> parent [

	^ parent
]

{ #category : #accessing }
SymbolTrieNode >> parent: anObject [

	parent := anObject
]

{ #category : #accessing }
SymbolTrieNode >> symbol [

	^ symbol
]

{ #category : #accessing }
SymbolTrieNode >> symbol: anObject [

	symbol := anObject
]
