transforming
checkParseTree
	| node |
	blockNodes := OrderedCollection new.
	node := self whichVariableNode: parseTree
				inInterval: interval
				name: name.
	node isNil
		ifTrue: [self refactoringFailure: 'Unable to locate node in parse tree'].
	definingNode := node whoDefines: name.
	definingNode isNil
		ifTrue: [self refactoringFailure: 'Cannot locate variable definition'].
	definingNode isSequence
		ifFalse: [self refactoringFailure: 'Variable is an argument'].
	(self usesDirectly: definingNode)
		ifTrue: [self refactoringFailure: 'Variable already bound tightly as possible'].
	(self checkNodes: (self subblocksIn: definingNode))
		ifFalse: [self refactoringError: 'Variable is possibly read before written']