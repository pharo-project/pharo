initialization
initializeRecursiveSelfRule
	RecursiveSelfRule := RBParseTreeSearcher new.
	RecursiveSelfRule 
		matchesAnyMethodOf: #(
			'`@methodName: `@args | `@temps | self `@methodName: `@args1' 
			'`@methodName: `@args | `@temps | ^ self `@methodName: `@args1')
		do: [ :node :answer | true ].
	^ RecursiveSelfRule