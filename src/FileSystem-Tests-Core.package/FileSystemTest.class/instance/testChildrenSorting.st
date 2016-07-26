tests
testChildrenSorting
	| directory sorted |
	
	directory := Path * 'plonk'.
	
	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'alfa'.
	filesystem createDirectory: directory / 'beta'.
	
	self markForCleanup: directory / 'alfa'.
	self markForCleanup: directory / 'beta'.
	self markForCleanup: directory.
	
	sorted := (filesystem childrenAt: directory) sorted.
	self assert: sorted size = 2.
	self assert: (sorted at: 1) basename = 'alfa'.
	self assert: (sorted at: 2) basename = 'beta'.