accessing
convertedFindString
	| specials |
	specials := '^$:\+*[](){}'.
	^String 
		streamContents: [:s | self findString 
			do: [:c | (specials includes: c) ifTrue: [s nextPut:$\].
				s nextPut: c]]