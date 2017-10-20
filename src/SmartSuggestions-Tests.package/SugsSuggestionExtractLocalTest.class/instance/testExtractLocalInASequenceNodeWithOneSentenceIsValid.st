tests - valid
testExtractLocalInASequenceNodeWithOneSentenceIsValid
	| oneLine context suggestion |
	
	oneLine := RBSequenceNode new.
	oneLine addNode: (RBVariableNode named: 'varName').
	context := SugsMockContext withNode: oneLine.
	
	suggestion := SugsSuggestionFactory createExtractToLocalVariable context: context.
	
	self assert: suggestion isValidForContext .