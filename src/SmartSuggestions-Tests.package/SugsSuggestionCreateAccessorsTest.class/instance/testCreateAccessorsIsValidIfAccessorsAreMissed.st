tests - instance variables
testCreateAccessorsIsValidIfAccessorsAreMissed
	| suggestion context |
	
	self addVariable: 'test' to: self emptyClass.
	context := self createContext: 'test'.
	suggestion := self createSuggestionAccesorsInstVar: context.
	
	self assert: suggestion isValidForContext.
	