user interface
packageListViewIn: aWindow
	^ aWindow
				newTextEditorFor: self
				getText: #getViewedPackageNames
				setText: #setViewedPackageNames:.
