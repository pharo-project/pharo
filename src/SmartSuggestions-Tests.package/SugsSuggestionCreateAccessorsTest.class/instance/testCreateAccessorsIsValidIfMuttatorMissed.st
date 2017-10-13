tests - instance variables
testCreateAccessorsIsValidIfMuttatorMissed
	| suggestion context |
	
	self addVariable: 'test' to: self emptyClass .
	self createGetter: 'test' in: self emptyClass .
	
	context := self createContext: 'test'.
	suggestion := self createSuggestionAccesorsInstVar: context.

	self assert: suggestion isValidForContext.
	
