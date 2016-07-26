styles
resultsPaneStyleFor: aBrick

	aBrick
		hSpaceFill: self resultsPaneWidth;
		vSpaceFill;
		with: [ :b | b band layout: GTSpotterStackedLayout new ]