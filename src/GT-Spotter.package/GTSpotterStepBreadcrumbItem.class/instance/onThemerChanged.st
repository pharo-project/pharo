brick-properties-events
onThemerChanged
	super onThemerChanged.
	
	self themer spotterThemer breadcrumbItemLabelWidgetStyleFor: self labelBrick.
	self themer spotterThemer breadcrumbItemTriangleWidgetStyleFor: self triangleBrick