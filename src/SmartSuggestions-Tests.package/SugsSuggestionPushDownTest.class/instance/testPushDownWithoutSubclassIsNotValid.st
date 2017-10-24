tests - methods
testPushDownWithoutSubclassIsNotValid
	| suggestion |

	self addMethodToDelegate.
	suggestion := self createSuggestionPushDown: (self createContext: self emptyClass ).
	
	self deny: suggestion isValidForContext