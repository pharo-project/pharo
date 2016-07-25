brick-layout
doLayoutForce

	self wrappedBounds
		markHeightDirty;
		markWidthDirty.
	
	self doLayout