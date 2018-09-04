"
I retrieve the symbols for the ecompletion algorithm.
"
Class {
	#name : #NECSymbolCache,
	#superclass : #Object,
	#classInstVars : [
		'cachedSymbols'
	],
	#category : #'NECompletion-Model'
}

{ #category : #private }
NECSymbolCache class >> cacheInterestingSymbolsAndDo: aBlock [
	"For time issue, we collect and perform the block in only one iteration"
	| cache |
	cache := OrderedCollection new: 5000.
	self interestingSymbolsDo: [:symbol |
		cache add: symbol.
		aBlock value: symbol ].
	" only assign the cache in the end, aBlock might do a non-local return!"
	cachedSymbols := cache.
]

{ #category : #private }
NECSymbolCache class >> cachedInterestingSymbolsDo: aBlock [
	self cachedSymbols do: aBlock
]

{ #category : #accessing }
NECSymbolCache class >> cachedSymbols [
	^cachedSymbols
]

{ #category : #query }
NECSymbolCache class >> contains: aString caseSensitive: aBoolean do: aBlock [ 
	| blockToPerform |

	blockToPerform := [ :each | 
		((each includes: $ ) not and: [ each includesSubstring: aString caseSensitive: aBoolean ]) 
			ifTrue: [ aBlock value: each ] ].

	cachedSymbols 
		ifNotNil:[ self cachedInterestingSymbolsDo: blockToPerform ]
		ifNil: [ self cacheInterestingSymbolsAndDo: blockToPerform ] 		

]

{ #category : #private }
NECSymbolCache class >> interestingSymbolsDo: aBlock [
	 Symbol selectorTable do: [:symbol | aBlock value: symbol ].
    Smalltalk globals keysDo: [:symbol | aBlock value: symbol ].
]

{ #category : #accessing }
NECSymbolCache class >> resetCachedSymbols [
	cachedSymbols := nil
]
