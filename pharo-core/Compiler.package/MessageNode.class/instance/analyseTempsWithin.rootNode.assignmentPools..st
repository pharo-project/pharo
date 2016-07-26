code generation (closures)
analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	"Assignments within optimized loops are tricky.  Because a loop repeats a
	 write to a temporary in an optimized loop effectively occurs after the loop.
	 To handle this collect the set of temps assigned to in optimized loops and
	 add extra writes after traversing the optimized loop constituents."
	| writtenToTemps |
	self isOptimizedLoop ifTrue:
		[{ receiver }, arguments do:
			[:node|
			(node notNil and: [node isBlockNode and: [node optimized]]) ifTrue:
				[assignmentPools at: node put: Set new]]].
	"receiver is nil in cascades"
	receiver == nil ifFalse:
		[receiver analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools].
	arguments do:
		[:node|
		node == nil ifFalse: "last argument of optimized to:do: can be nil"
			[node analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools]].
	"Add assignments representing subsequent iterations
	 and redo the closure analysis for the written-to temps."
	self isOptimizedLoop ifTrue:
		[writtenToTemps := Set new.
		 { receiver }, arguments do:
			[:node|
			(node notNil and: [node isBlockNode and: [node optimized]]) ifTrue:
				[(assignmentPools removeKey: node) do:
					[:temp|
					temp isBlockArg ifFalse: "ignore added assignments to to:do: loop args"
						[writtenToTemps add: temp.
						 temp addWriteWithin: node at: rootNode locationCounter]]]].
		 writtenToTemps isEmpty ifFalse:
			[(writtenToTemps asSortedCollection: ParseNode tempSortBlock) do:
				[:each| each analyseClosure: rootNode].
			 (writtenToTemps collect: [:each| each definingScope]) do:
				[:blockNode|
				blockNode ifHasRemoteTempNodeEnsureInitializationStatementExists: rootNode]]]