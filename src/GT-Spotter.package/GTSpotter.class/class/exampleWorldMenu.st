example
exampleWorldMenu
	
	^ GTSpotterMorph new
		extent: 400@500;
		doLayout;
		spotterModel: (self on: World worldMenu);
		openCenteredInWorld