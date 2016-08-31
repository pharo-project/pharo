testing
selectorAndClassAreCorrect
	| class |
	methodSearchField text
		ifNotNil:
			[ :method | 
			classSearchField text
				ifNotNil:
					[ 
					class := self class environment at: classSearchField text asSymbol ifAbsent: [ ^ false].
					(class includesSelector: method asSymbol)
								ifTrue: [ ^ true ]]].
	^ false 