tests
testPathToMissingAncestor
	self assert: (self tree allAncestorsOnPathTo: MCVersionInfo new) isEmpty