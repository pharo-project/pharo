initialize-release
statements: statementsCollection returns: returnBool 
	"Decompile."

	| returnLast |
	returnLast := returnBool.
	returns := false.
	statements := 
		(statementsCollection size > 1 
			and: [(statementsCollection at: statementsCollection size - 1) 
					isReturningIf])
				ifTrue: 
					[returnLast := false.
					statementsCollection allButLast]
				ifFalse: [statementsCollection size = 0
						ifTrue: [Array with: NodeNil]
						ifFalse: [statementsCollection]].
	arguments := #().
	temporaries := #().
	optimized := false.
	returnLast ifTrue: [self returnLast]