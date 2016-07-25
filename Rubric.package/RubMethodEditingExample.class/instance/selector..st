accessing
selector: aSymbol
	selector := aSymbol ifNotNil: [:sel |( sel copyWithout: Character cr) asSymbol].
	self updateCodeWith: ''.
	self changed: #selector.
	self classToUse 
		ifNotNil: [:cls | (cls selectors includes: selector)
			ifTrue: [self updateCodeWith:  (cls sourceCodeAt: selector)]]