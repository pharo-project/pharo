Class {
	#name : #SymbolTrie,
	#superclass : #Object,
	#instVars : [
		'roots'
	],
	#category : #'Collections-Strings-Base'
}

{ #category : #'instance creation' }
SymbolTrie class >> withAll: aCollectionOfSymbols [

	^ self new
		  addAll: aCollectionOfSymbols;
		  yourself
]

{ #category : #adding }
SymbolTrie >> add: aString [

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

{ #category : #accessing }
SymbolTrie >> getSymbol: aString [

	^ self roots getSymbol: aString
]

{ #category : #testing }
SymbolTrie >> includes: aString [

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
