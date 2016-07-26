accessing
sharedPools
	^ self allPoolDictionaryNames collect: [ :each | Smalltalk globals at: each asSymbol ifAbsent: [ Dictionary new ] ]