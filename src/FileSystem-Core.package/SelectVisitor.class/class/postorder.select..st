instance creation
postorder: aReference select: aBlock
	^ (self select: aBlock) postorder: aReference