search
handleExceptionalSelectors
	"Handle some very slippery selectors.
	asSymbol -- want to be able to produce it, but do not want to make every string submitted into a Symbol!"

	| aSel |
	answers first isSymbol
		ifFalse: [ ^ self ].
	thisData first first isString
		ifFalse: [ ^ self ].
	aSel := #asSymbol.
	(self testPerfect: aSel)
		ifFalse: [ ^ self ].
	selector add: aSel.
	expressions
		add:
			(String
				streamContents: [ :strm | 
					strm nextPutAll: 'data' , argMap first printString.
					aSel keywords
						doWithIndex: [ :key :ind | 
							strm nextPutAll: ' ' , key.
							key last == $: | key first isLetter not
								ifTrue: [ strm nextPutAll: ' data' , (argMap at: ind + 1) printString ] ] ])