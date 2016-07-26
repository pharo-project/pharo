private
renderPane: aPane inUI: aMorph from: aDashboard
	aMorph addMorph: (self renderObject: aPane) fullFrame: (aDashboard layoutOfPaneNamed: aPane name)