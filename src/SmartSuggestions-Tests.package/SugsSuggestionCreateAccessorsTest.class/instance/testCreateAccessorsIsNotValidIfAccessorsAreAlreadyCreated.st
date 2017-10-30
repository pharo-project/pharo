tests - instance variables
testCreateAccessorsIsNotValidIfAccessorsAreAlreadyCreated
	| suggestion context |
	
	self addVariable: 'test' to: self emptyClass.
	self createAccessorsFor: 'test' in: self emptyClass.
	context := self createContext: 'test'.
	suggestion := self createSuggestionAccesorsInstVar: context.
	
	self deny: suggestion isValidForContext