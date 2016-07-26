example
exampleInWindow
	
	^ GTSpotterMorph new
		extent: 400@500;
		doLayout;
		spotterModel: self new;
		openInWindow