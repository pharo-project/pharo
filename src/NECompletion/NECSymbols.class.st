"
I retrieve the symbols for the ecompletion algorithm.

TODO remove stupid symbols (like packages and timestamps)
"
Class {
	#name : #NECSymbols,
	#superclass : #Object,
	#classInstVars : [
		'cachedSymbols'
	],
	#category : #'NECompletion-Model'
}

{ #category : #private }
NECSymbols class >> cacheInterestingSymbolsAndDo: aBlock [
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
NECSymbols class >> cachedInterestingSymbolsDo: aBlock [
	self cachedSymbols do: aBlock
]

{ #category : #accessing }
NECSymbols class >> cachedSymbols [
	^cachedSymbols
]

{ #category : #query }
NECSymbols class >> contains: aString caseSensitive: aBoolean do: aBlock [ 
	| blockToPerform |

	blockToPerform := [ :each | 
		((each includes: $ ) not and: [ each includesSubstring: aString caseSensitive: aBoolean ]) 
			ifTrue: [ aBlock value: each ] ].

	cachedSymbols 
		ifNotNil:[ self cachedInterestingSymbolsDo: blockToPerform ]
		ifNil: [ self cacheInterestingSymbolsAndDo: blockToPerform ] 		

]

{ #category : #private }
NECSymbols class >> interestingSymbolsDo: aBlock [

	Symbol allSymbolTablesDo:[:symbol |
		symbol isEmpty
			ifFalse: [
				(symbol first isUppercase and:[ Smalltalk globals includesKey: symbol ])
					ifTrue:[ aBlock value: symbol ]
					ifFalse: [ symbol isSelectorSymbol ifTrue: [ aBlock value: symbol ]]]].
]

{ #category : #accessing }
NECSymbols class >> resetCachedSymbols [
	cachedSymbols := nil
]

{ #category : #query }
NECSymbols class >> startsWith: aChar caseSensitive: aBoolean do: aBlock [ 
	| char caseInSensitive firstChar |
	caseInSensitive := aBoolean not.
	firstChar := caseInSensitive 
		ifTrue: [ aChar asLowercase ]
		ifFalse: [ aChar ].
	Symbol allSymbolTablesDo: 
		[ :each | 
		| size |
		size := each size.
		char := size > 0 ifTrue: [ each first ].
		(char notNil 
			and: [ (char == firstChar or: [ caseInSensitive and: [ char asLowercase == firstChar ] ])
			and: [ (each 
				findAnySubstring: '- '
				startingAt: 2) > size ] ]) ifTrue: [ aBlock value: each ] ]
]
