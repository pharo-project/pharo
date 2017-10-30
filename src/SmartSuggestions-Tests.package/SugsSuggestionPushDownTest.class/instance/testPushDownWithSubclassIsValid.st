tests - methods
testPushDownWithSubclassIsValid
	| suggestion subclass |

	subclass := self createSubclassFrom: self emptyClass named: #TestSuggestionsSubclass.
	self addMethodToDelegate.
	suggestion := self createSuggestionPushDown: (self createContext: self emptyClass ).
	
	self assert: suggestion isValidForContext.
	self removeClass: subclass.