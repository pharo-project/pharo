api
configurePane: aBrick
	|brick|
	brick := aBrick asBrick.
	brick
		layoutProperties: GLMBrickLayoutProperties new;
		"width of tab is proportional to number of visible panes and my width"
		width: [ self band width / self band subbricksCount ];
		vSpaceFill;
		marginLeft: self paneMargin;
		marginRight: self paneMargin.
		
	brick layout widthDependency: self band.
	^ brick