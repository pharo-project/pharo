query
contains: aString caseSensitive: aBoolean do: aBlock 
	| blockToPerform |

	blockToPerform := [ :each | 
		((each includes: $ ) not and: [ each includesSubstring: aString caseSensitive: aBoolean ]) 
			ifTrue: [ aBlock value: each ] ].

	cachedSymbols 
		ifNotNil:[ self cachedInterestingSymbolsDo: blockToPerform ]
		ifNil: [ self cacheInterestingSymbolsAndDo: blockToPerform ] 		
