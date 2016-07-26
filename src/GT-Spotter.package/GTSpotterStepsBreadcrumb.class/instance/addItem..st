adding/removing
addItem: aBrick

	aBrick width: [ :itemBrick | self calculateItemWidth: itemBrick ].
	aBrick layout widthDependency: self.
	
	self items size = 1 ifTrue: [
		self items first labelBrick paddingLeft: self themer spotterThemer breadcrumbSeparatorWidth / 2 ].
	
	self items addLast: aBrick.
	self band addBrickBack: aBrick.
	self items do: [ :b | b wrappedBounds markWidthDirty ].
	self band doLayoutForce.