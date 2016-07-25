doits
preLoad: packageOrVersionSpec

	| block |
	(block := packageOrVersionSpec preLoadDoItBlock) ~~ nil
		ifTrue: [ block valueWithPossibleArgs: { self. packageOrVersionSpec. } ]