widget method
signatureFor: aSelector

	^ (aSelector includes: $:)
		ifTrue: [ | col |
			col := aSelector substrings: ':'.
			String streamContents: [:s |
					col doWithIndex: [:e :i |
						i == 1 
							ifTrue: [ s << e << $: << ' anObject ' ]
							ifFalse: [ s << e << $: << ' anObject'<< (i-1) printString << ' ' ]]]]
		ifFalse: [ aSelector ]