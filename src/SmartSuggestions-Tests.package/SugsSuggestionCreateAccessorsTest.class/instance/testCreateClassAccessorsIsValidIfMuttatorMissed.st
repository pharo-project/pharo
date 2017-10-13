tests - class variables
testCreateClassAccessorsIsValidIfMuttatorMissed
	| suggestion context |
	
	self addVariable: 'Test' to: self emptyClass .
	self createGetter: 'Test' in: self emptyClass .
	
	context := self createContext: 'Test'.
	suggestion := self createSuggestionAccesorsInstVar: context.

	self assert: suggestion isValidForContext.
	
