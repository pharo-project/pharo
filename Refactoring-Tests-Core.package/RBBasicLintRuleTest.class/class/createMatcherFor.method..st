private
createMatcherFor: codeStrings method: aBoolean 
	| matcher |
	matcher := RBParseTreeSearcher new.
	aBoolean
		ifTrue: [matcher addMethodSearches: codeStrings -> [:aNode :answer | true]]
		ifFalse: [matcher addSearches: codeStrings -> [:aNode :answer | true]].
	^matcher