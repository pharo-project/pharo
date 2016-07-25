user interface
open
	
	Cursor wait
		showWhile: [ | window |window := StandardWindow new model: self.
			window title: self windowTitle.
			self buildWindowIn: window.
			window themeChanged.
			window openInWorld.
			(window findDeeplyA: self treeMorphClass)  takeKeyboardFocus. 
			^ window]