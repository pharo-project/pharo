layouting
updateHeight: aBrick on: aLayout

	aBrick withHeight: (aLayout normalizeHeight: (aLayout heightChildrenDependency: aBrick) of: aBrick)