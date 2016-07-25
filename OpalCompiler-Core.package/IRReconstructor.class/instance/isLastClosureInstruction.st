testing
isLastClosureInstruction
	| nextJumps |
	nextJumps := jumpAheadStacks at: sourceMapByteIndex + 1 ifAbsent: [ ^ false ].
	^ nextJumps anySatisfy: [ :anOrigin | anOrigin = self currentScope ]