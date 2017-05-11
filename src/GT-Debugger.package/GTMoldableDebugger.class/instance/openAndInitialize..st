actions
openAndInitialize: aNewDebugger
	
	aNewDebugger color: self color.
	aNewDebugger openWithFullView.
	aNewDebugger window window "Hack to get this working with SpecDebugger"
		position: self window position;
		extent: self window extent.
	^ aNewDebugger