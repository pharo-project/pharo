tests - methods
testRemoveArgsWithoutArgsItsNotValid
	| suggestion |

	suggestion := SugsSuggestionFactory createRemoveParameter context: (SugsMockContext withMessageName: 'methodNoParamas' ); yourself.
		
	self deny: suggestion isValidForContext