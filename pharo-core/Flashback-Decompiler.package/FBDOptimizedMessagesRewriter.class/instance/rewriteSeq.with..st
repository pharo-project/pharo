rewriting
rewriteSeq: seq with: currentCascade
	currentCascade size > 1
		ifTrue: [ self rewriteStatements: currentCascade in: seq ].
		currentCascade removeAll