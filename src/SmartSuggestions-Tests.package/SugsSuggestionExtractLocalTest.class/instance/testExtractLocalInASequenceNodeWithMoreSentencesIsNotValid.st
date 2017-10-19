tests - valid
testExtractLocalInASequenceNodeWithMoreSentencesIsNotValid
	| multipleLines context suggestion |
	
	multipleLines := RBSequenceNode new.
	multipleLines addNode: (RBVariableNode named: 'varName').
	multipleLines addNode: (RBVariableNode named: 'otherName').
	context := SugsMockContext withNode: multipleLines.
	
	suggestion := SugsSuggestionFactory createExtractToLocalVariable context: context.
	
	self deny: suggestion isValidForContext .
	