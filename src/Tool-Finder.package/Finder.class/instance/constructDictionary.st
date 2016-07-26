private
constructDictionary
	"I construct the adequate dictionary regarding the search mode"

	self searchingString isEmpty ifTrue: [ ^ self resultDictionary: Dictionary new].
	[ :job|
		job title: 'Searching...'.
		self isSelectorsSymbol
			ifTrue: [ self constructDictionaryWithMessagesNameSearch: self searchingString].
		self isClassNamesSymbol
			ifTrue: [ self constructClassNamesDictionary].
		self isSourceSymbol
			ifTrue: [ self constructSourceDictionary].
		self isExamplesSymbol
			ifTrue: [ self constructDictionaryWithMethodFinder: self searchingString].
		self isPragmasSymbol
			ifTrue: [ self constructDictionaryWithPragmaSearch: self searchingString ].
	] asJob run