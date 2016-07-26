instance creation
findInternedSelector: aString
	| symbol |
	symbol := self findInterned: aString.
	"if it isn't found or not a SelectorSymbol - return nil"
	(symbol isNil or: [ symbol isSelectorSymbol not ])
		ifTrue: [ ^ nil ].
	"otherwise, return this symbol"
	^ symbol