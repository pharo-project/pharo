tests - source
testDelegateSelectionWithInstaceVariablesItsValid
	| suggestion |

	self addVariable: 'test' to: self emptyClass.
	self addMethodToDelegate.
	suggestion := self createSuggestionDelegate: self createContexForDelegate .
	
	self assert: suggestion isValidForContext