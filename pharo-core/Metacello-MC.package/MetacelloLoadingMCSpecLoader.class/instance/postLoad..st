doits
postLoad: packageOrVersionSpec

	| block |
	(block := packageOrVersionSpec postLoadDoItBlock) ~~ nil
		ifTrue: [ block valueWithPossibleArgs: { self. packageOrVersionSpec. } ]