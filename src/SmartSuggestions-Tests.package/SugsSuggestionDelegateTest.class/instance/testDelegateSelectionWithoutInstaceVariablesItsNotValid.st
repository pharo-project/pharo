tests - source
testDelegateSelectionWithoutInstaceVariablesItsNotValid
	| suggestion |
	
	self addMethodToDelegate.
	suggestion := self createSuggestionDelegate: self createContexForDelegate.
	
	self deny: suggestion isValidForContext