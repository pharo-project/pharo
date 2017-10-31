tests - methods
testRemoveArgsWithArgsItsValid
	| suggestion |

	suggestion := SugsSuggestionFactory createRemoveParameter context: (SugsMockContext withMessageName: 'methodNoParamas:'  ); yourself.
	
	self assert: suggestion isValidForContext
	