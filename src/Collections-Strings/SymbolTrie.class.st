Class {
	#name : #SymbolTrie,
	#superclass : #Object,
	#instVars : [
		'roots'
	],
	#category : #'Collections-Strings-Base'
}

{ #category : #adding }
SymbolTrie >> add: aString [

	aString ifEmpty: [ ^ self ].
	^ self roots add: aString
]

{ #category : #adding }
SymbolTrie >> addAll: aCollection [

	aCollection do: [ :string | self add: string ]
]

{ #category : #accessing }
SymbolTrie >> allSymbols [

	^ self roots addAllSymbolsTo: OrderedCollection new
]

{ #category : #enumerating }
SymbolTrie >> do: aBlock [

	self roots symbolsDo: aBlock
]

{ #category : #testing }
SymbolTrie >> includes: aString [

	aString ifEmpty: [ self error: 'Cannot contain empty string' ].

	^ self roots includes: aString
]

{ #category : #initialization }
SymbolTrie >> initialize [

	super initialize.
	self roots: SymbolTrieNode new
]

{ #category : #accessing }
SymbolTrie >> roots [

	^ roots
]

{ #category : #accessing }
SymbolTrie >> roots: anObject [

	roots := anObject
]
