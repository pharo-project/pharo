tests
testParentResolutionWithRemoteReference
	| base relative absolute |
	base := (filesystem / 'plonk' / 'pinto').
	relative := (FileSystem memory referenceTo: '../griffle/zonk').
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: (absolute path at: 1) = 'plonk'.
	self assert: (absolute path at: 2) = 'griffle'.
	self assert: (absolute path at: 3) = 'zonk'.
	
	