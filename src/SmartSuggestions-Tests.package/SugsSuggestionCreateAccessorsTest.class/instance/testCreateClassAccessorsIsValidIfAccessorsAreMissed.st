tests - class variables
testCreateClassAccessorsIsValidIfAccessorsAreMissed
	| suggestion context |
	
	self addVariable: 'Test' to: self emptyClass.
	context := self createContext: 'Test'.
	suggestion := self createSuggestionAccesorsInstVar: context.
	
	self assert: suggestion isValidForContext.
	