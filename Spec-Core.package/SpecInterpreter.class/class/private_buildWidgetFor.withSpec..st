private
private_buildWidgetFor: aComposableModel withSpec: aSymbol
	
	^ self private_interpretASpec: (aComposableModel retrieveSpec: aSymbol) model: aComposableModel selector: aSymbol