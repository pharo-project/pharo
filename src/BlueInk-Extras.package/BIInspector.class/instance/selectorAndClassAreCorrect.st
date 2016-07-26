testing
selectorAndClassAreCorrect
	| class |
	methodSearchField text
		ifNotNil:
			[ :method | 
			classSearchField text
				ifNotNil:
					[ 
					(SmalltalkImage current hasClassNamed: classSearchField text)
						ifTrue:
							[ 
							class := classSearchField text asClass.
							(class methodDict includesKey: method asSymbol)
								ifTrue: [ ^ true ] ] ] ].
	^ false 