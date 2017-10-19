tests - class variables
testCreateClassAccessorsIsValidIfAccessIsMissed
	| suggestion context |
	
	self addVariable: 'Test' to: self emptyClass.
	self createSetter: 'Test' in: self emptyClass.
	context := self createContext: 'Test'.
	suggestion := self createSuggestionAccesorsInstVar: context.
	
	self assert: suggestion isValidForContext.
