tests
testParentResolutionWithRemoteReference
	| base relative absolute |
	base := (filesystem / 'plonk' / 'pinto').
	relative := (FileSystem memory referenceTo: '../griffle/zonk').
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: absolute path segments equals: #('plonk' 'pinto' '..' 'griffle' 'zonk').
	
	