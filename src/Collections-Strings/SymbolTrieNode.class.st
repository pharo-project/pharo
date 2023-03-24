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
SymbolTrieNode class >> parent: aSymbolTrieNode [

	^ self new
		  parent: aSymbolTrieNode;
		  yourself
]

{ #category : #adding }
SymbolTrieNode >> add: aString [

	^ self addIn: aString symbol: aString createSymbol
]

{ #category : #adding }
SymbolTrieNode >> addAllSymbolsTo: aCollection [

	self isSymbol ifTrue: [ aCollection add: self symbol ].
	self keysAndValuesDo: [ :letter :node | node addAllSymbolsTo: aCollection ].
	^ aCollection
]

{ #category : #adding }
SymbolTrieNode >> addIn: aString symbol: symbolToAdd [

	aString size = 1 ifTrue: [
		self symbol: symbolToAdd.
		^ symbolToAdd ].

	(self at: aString first ifAbsentPut: [ self class parent: self ]) addIn: aString allButFirst symbol: symbolToAdd
]

{ #category : #accessing }
SymbolTrieNode >> children [

	^ children
]

{ #category : #accessing }
SymbolTrieNode >> children: anObject [

	children := anObject
]

{ #category : #adding }
SymbolTrieNode >> getSymbol: aString [

	aString size = 1 ifTrue: [
		^ self isSymbol
			  ifTrue: [ self symbol ]
			  ifFalse: [ nil ] ].

	^ self
		  at: aString first
		  ifPresent: [ :node | node getSymbol: aString allButFirst ]
		  ifAbsent: [ nil ]
]

{ #category : #testing }
SymbolTrieNode >> includes: aString [

	| node |
	node := self at: aString first ifAbsent: [ ^ false ].

	aString size = 1 ifTrue: [ ^ node isSymbol ].

	^ node includes: aString allButFirst
]

{ #category : #testing }
SymbolTrieNode >> initialize: aSize [
	super initialize: aSize.
	symbol := WeakArray new: 1.
]

{ #category : #testing }
SymbolTrieNode >> isSymbol [

	^ self symbol isNotNil
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

	^ symbol at: 1
]

{ #category : #accessing }
SymbolTrieNode >> symbol: anObject [

	symbol at: 1 put: anObject
]

{ #category : #enumerating }
SymbolTrieNode >> symbolsDo: aBlock [

	self isSymbol ifTrue: [ aBlock value: self symbol ].
	self valuesDo: [ :value | value symbolsDo: aBlock ]
]
