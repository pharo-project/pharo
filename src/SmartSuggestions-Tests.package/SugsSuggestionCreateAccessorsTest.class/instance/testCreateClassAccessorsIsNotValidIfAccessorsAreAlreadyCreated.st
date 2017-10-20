tests - class variables
testCreateClassAccessorsIsNotValidIfAccessorsAreAlreadyCreated
	| suggestion context |
	
	self addVariable: 'Test' to: self emptyClass.
	self createAccessorsFor: 'Test' in: self emptyClass.
	context := self createContext: 'Test'.
	suggestion := self createSuggestionAccesorsClassVar: context.
	
	self deny: suggestion isValidForContext