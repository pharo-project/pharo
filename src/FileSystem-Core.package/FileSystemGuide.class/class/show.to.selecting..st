instance creation
show: aReference to: aVisitor selecting: aBlock
	^ (self for: aVisitor)
		selectChildren:  aBlock;
		show: aReference