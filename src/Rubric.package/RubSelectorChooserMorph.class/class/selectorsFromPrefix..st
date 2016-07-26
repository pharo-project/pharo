accessing
selectorsFromPrefix: aPrefix
	"returns the value of labels from a selector prefix"
	"self selectorsFromPrefix: 'prin' "
	| key applicants |
	aPrefix isEmptyOrNil  ifTrue: [^ #()].
	applicants := aPrefix first canBeGlobalVarInitial
		ifTrue: [Array streamContents: [:strm |
			Symbol allSymbolTablesDo: [:each |
				(each notEmpty and: [each first canBeGlobalVarInitial])
					ifTrue: [strm nextPut: each]]]]
		ifFalse: [Array streamContents: [:strm |
			Symbol allSymbolTablesDo: [:each |
				(each notEmpty and: [each first canBeGlobalVarInitial not])
					ifTrue: [strm nextPut: each]]]].
	key := aPrefix asLowercase.
	^ (applicants select: [:each | (each asLowercase beginsWith: key) and: [each noneSatisfy: [:c | c tokenish not]]]) sort
