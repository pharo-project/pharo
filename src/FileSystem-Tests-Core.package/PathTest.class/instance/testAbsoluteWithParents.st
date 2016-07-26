tests
testAbsoluteWithParents
	| path allPaths |
	path := Path / 'plonk' / 'griffle' / 'nurb'.
	allPaths := path withParents.
	
	self assert: allPaths size equals: 4.
	self assert: allPaths first isRoot.
	self assert: allPaths second basename equals: 'plonk'.
	self assert: allPaths second size equals: 1.
	self assert: (allPaths second isChildOf: allPaths first).
	self assert: allPaths third basename equals: 'griffle'.
	self assert: allPaths third size equals: 2.
	self assert: (allPaths third isChildOf: allPaths second).
	self assert: allPaths fourth basename equals: 'nurb'.
	self assert: allPaths fourth size equals: 3.
	self assert: (allPaths fourth isChildOf: allPaths third).
	
	self assert: allPaths fourth equals: path.
	self assert: allPaths fourth == path