instance creation
show: aString from: startNumber to: endNumber during: aBlock
	self flag: 'When we go from Exceptions to announcements, this will be the primary entry point'.

	^ self uniqueInstance show: aString from: startNumber to: endNumber during: aBlock.