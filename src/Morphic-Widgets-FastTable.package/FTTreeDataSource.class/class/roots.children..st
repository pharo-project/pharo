instance creation
roots: aCollection children: aBlock
	^ self root: (self rootItemFor: aCollection) children: aBlock