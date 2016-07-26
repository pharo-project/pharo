class initialization
initializeAfterLoad1
	RecursiveSelfRule := RBParseTreeSearcher new.
	RecursiveSelfRule
		addMethodSearches: #('`@methodName: `@args | `@temps | self `@methodName: `@args' '`@methodName: `@args | `@temps | ^self `@methodName: `@args')
				-> [:aNode :answer | true]