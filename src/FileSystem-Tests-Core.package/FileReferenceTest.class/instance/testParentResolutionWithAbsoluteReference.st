tests
testParentResolutionWithAbsoluteReference
	| base relative absolute |
	base := (filesystem / '/plonk' / 'pinto').
	relative := (FileSystem memory / 'griffle' / 'zonk').
	absolute := base resolve: relative.
	self assert: absolute fileSystem == relative fileSystem.
	self assert: absolute isAbsolute.
	self assert: (absolute path at: 1) = 'griffle'.
	self assert: (absolute path at: 2) = 'zonk'.
	
	