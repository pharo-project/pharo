searching
forSelectorsDo: selectorBlock forClassNamesDo: classNamesBlock forSourceDo: sourceBlock forExamplesDo: exampleBlock forPragmasDo: pragmaBlock

	self isSelectorsSymbol
		ifTrue: [^selectorBlock value].
	self isClassNamesSymbol
		ifTrue: [^classNamesBlock value].
	self isSourceSymbol
		ifTrue: [^sourceBlock value].
	self isExamplesSymbol
		ifTrue: [^exampleBlock value].
	self isPragmasSymbol
		ifTrue: [^pragmaBlock value].