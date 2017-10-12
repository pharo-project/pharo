tests - valid
testExtractLocalInALiteralNodeIsValid
	| literal context suggestion |

	literal := RBLiteralValueNode  new.
	context := SugsMockContext withNode: literal.
	suggestion := SugsSuggestionFactory createExtractToLocalVariable context: context.	
	self assert: suggestion isValidForContext .