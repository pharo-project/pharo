Class {
	#name : 'SymbolTable',
	#superclass : 'Object',
	#instVars : [
		'newSymbols',
		'symbolTable',
		'selectorTable'
	],
	#classVars : [
		'SystemSymbolTable'
	],
	#category : 'Collections-Strings-Base',
	#package : 'Collections-Strings',
	#tag : 'Base'
}

{ #category : 'instance creation' }
SymbolTable class >> fromSymbols: aListOfSymbols [

	| table |
	table := self new.
	aListOfSymbols do: [ :e | table intern: e ].
	table compactSymbols.
	^ table
		
]

{ #category : 'accessing' }
SymbolTable class >> system [

	^ SystemSymbolTable ifNil: [ SystemSymbolTable := self new ]
]

{ #category : 'accessing' }
SymbolTable >> allSymbols [
	"Answer all interned symbols"

	^ Array streamContents: [ :s |
			  s nextPutAll: newSymbols.
			  s nextPutAll: symbolTable ]
]

{ #category : 'enumerating' }
SymbolTable >> allSymbolsDo: aBlock [

	newSymbols do: aBlock.
	symbolTable do: aBlock
]

{ #category : 'private' }
SymbolTable >> basicCreateSymbolFrom: aSymbolOrString [

	^ ((aSymbolOrString isOctetString
		    ifTrue: [ ByteSymbol ]
		    ifFalse: [ WideSymbol ]) basicNew: aSymbolOrString size) string: aSymbolOrString
]

{ #category : 'as yet unclassified' }
SymbolTable >> compactSymbols [

	"Compact all elements in newSymbols into the main symbolTable"
	
	symbolTable addAll: newSymbols.
	newSymbols := WeakSet new
]

{ #category : 'interning' }
SymbolTable >> findInterned: aStringOrSymbol [

	^ (symbolTable like: aStringOrSymbol) ifNil: [ newSymbols like: aStringOrSymbol ]
]

{ #category : 'testing' }
SymbolTable >> hasInterned: aStringOrSymbol [
	"Answer with false if aString hasnt been interned (into a Symbol)"

	^ (symbolTable includes: aStringOrSymbol) or: [ newSymbols includes: aStringOrSymbol ]
]

{ #category : 'initialization' }
SymbolTable >> initialize [

	super initialize.
	symbolTable := WeakSet new.
	newSymbols := WeakSet new.
]

{ #category : 'interning' }
SymbolTable >> intern: aStringOrSymbol [

	SymbolTableSemaphore enabled ifFalse: [
		^ self rawIntern: aStringOrSymbol ].
	^ SymbolTableSemaphore singleInstance critical: [ self rawIntern: aStringOrSymbol ]
]

{ #category : 'testing' }
SymbolTable >> isEmpty [
	
	^ newSymbols isEmpty and: [ symbolTable isEmpty ]
]

{ #category : 'private' }
SymbolTable >> rawIntern: aStringOrSymbol [

	^ (self findInterned: aStringOrSymbol) ifNil: [
		  newSymbols add: (self basicCreateSymbolFrom: aStringOrSymbol) ]
]

{ #category : 'private' }
SymbolTable >> rebuildSelectorTable [

	^ SystemNavigation new allMethods
		  collect: [ :method | method selector ]
		  as: WeakIdentitySet
]

{ #category : 'selectors' }
SymbolTable >> recordSelector: aSymbol [

	self selectorTable add: aSymbol
]

{ #category : 'accessing' }
SymbolTable >> rehash [
   "Symbol rehash"
	"Rebuild the hash table, reclaiming unreferenced Symbols."

	symbolTable := WeakSet withAll: self allSymbols.
	newSymbols := WeakSet new.
	self resetSelectorTable
]

{ #category : 'accessing' }
SymbolTable >> resetSelectorTable [
	
	selectorTable := nil
]

{ #category : 'selectors' }
SymbolTable >> selectorTable [

	^ selectorTable ifNil: [ selectorTable := self rebuildSelectorTable ]
]

{ #category : 'selectors' }
SymbolTable >> selectorThatStartsCaseSensitive: leadingCharacters skipping: skipSym [
	"Same as thatStartsCaseSensitive:skipping: but on the SelectorTable only"
	| size firstMatch key |

	size := leadingCharacters size.
	size = 0 ifTrue: [^skipSym ifNil: [#''] ifNotNil: [nil]].
	firstMatch := leadingCharacters at: 1.
	size > 1 ifTrue: [key := leadingCharacters copyFrom: 2 to: size].
	self selectorTable do: [:each |
			(each isNotNil and: [each size >= size]) ifTrue:
				[
					((each at: 1) == firstMatch and:
						[key == nil or:
							[(each findString: key startingAt: 2 caseSensitive: true) = 2]])
								ifTrue: [^each]
				]
		] after: skipSym.

	^nil
]

{ #category : 'adding' }
SymbolTable >> unsafeAddAll: aListOfSymbols [

	symbolTable addAll: aListOfSymbols
]
