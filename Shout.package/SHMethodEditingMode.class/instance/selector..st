accessing
selector: aSymbol
	selector := aSymbol ifNotNil: [:sel |( sel copyWithout: Character cr) asSymbol].
	self changed: #selector.
	self classToUse 
		ifNotNil: [:cls | (cls selectors includes: selector)
			ifTrue: [self code: (cls sourceCodeAt: selector)]]