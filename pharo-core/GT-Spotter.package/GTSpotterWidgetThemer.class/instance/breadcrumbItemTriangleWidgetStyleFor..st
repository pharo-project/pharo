styles-widget
breadcrumbItemTriangleWidgetStyleFor: aBrick

	aBrick
		renderer: (GTSpotterTriangleRenderer new orientation: #bottom).
		
	aBrick owner isTriangleVisible
		ifTrue:[ aBrick color: self breadcrumbItemBackgroundColor ]
		ifFalse: [ aBrick color: Color transparent ]