tests - valid
testExtractLocalInAMessageNodeIsValid
	| context suggestion |
	context := SugsMockContext withMessageName: 'justAName'.
	suggestion := SugsSuggestionFactory createExtractToLocalVariable context: context.
	self assert: suggestion isValidForContext
