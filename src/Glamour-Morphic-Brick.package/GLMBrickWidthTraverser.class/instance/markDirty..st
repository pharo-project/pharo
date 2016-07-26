private
markDirty: aBrick

	self markLayoutDirty: aBrick owner.
	aBrick markWidthDirty
	