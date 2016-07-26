tests
testSimpleResolution
	| base relative absolute |
	base := filesystem / 'plonk'.
	relative := (Path * 'griffle') / 'zonk'.
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: (absolute path at: 1) = 'plonk'.
	self assert: (absolute path at: 2) = 'griffle'.
	self assert: (absolute path at: 3) = 'zonk'.
	
	