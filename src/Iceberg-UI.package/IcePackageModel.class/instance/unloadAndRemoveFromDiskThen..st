actions
unloadAndRemoveFromDiskThen: aBlock
	self unload.
	self removeFromDiskThen: [].
	aBlock value