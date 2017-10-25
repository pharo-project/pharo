tests - instance variables
testCreateAccessorsIsValidIfAccessIsMissed
	| suggestion context |
	
	self addVariable: 'test' to: self emptyClass.
	self createSetter: 'test' in: self emptyClass.
	context := self createContext: 'test'.
	suggestion := self createSuggestionAccesorsInstVar: context.
	
	self assert: suggestion isValidForContext.
