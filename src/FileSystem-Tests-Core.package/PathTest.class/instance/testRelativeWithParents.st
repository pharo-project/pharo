tests
testRelativeWithParents
	| path allPaths |
	path := Path * 'plonk' / 'griffle' / 'nurb'.
	allPaths := path withParents.
	
	self assert: allPaths size equals: 3.
	self assert: allPaths first basename equals: 'plonk'.
	self assert: allPaths first size equals: 1.
	self assert: allPaths second basename equals: 'griffle'.
	self assert: allPaths second size equals: 2.
	self assert: (allPaths second isChildOf: allPaths first).
	self assert: allPaths third basename equals: 'nurb'.
	self assert: allPaths third size equals: 3.
	self assert: (allPaths third isChildOf: allPaths second).
	self assert: allPaths third == path