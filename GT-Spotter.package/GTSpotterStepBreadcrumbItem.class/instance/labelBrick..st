accessing
labelBrick: aBrick

	labelBrick := aBrick.
	
	self themer spotterThemer breadcrumbItemLabelStyleFor: self labelBrick.
	self addBrickBack: self labelBrick