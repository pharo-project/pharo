instance creation
postorder: aReference collect: aBlock
	^ (self collect: aBlock) postorder: aReference