testing
assertFileOutFrom: writerClass canBeFiledInWith: aBlock
	(writerClass on: stream) writeSnapshot: self mockSnapshot.
	self alterInitialState.
	self assertSuccessfulLoadWith: aBlock.
	self mockPackage unload.
	self assertSuccessfulLoadWith: aBlock.
