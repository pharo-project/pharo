instance creation
preorder: aReference collect: aBlock
	^ (self collect: aBlock) preorder: aReference