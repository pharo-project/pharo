private
private_buildWidgetFor: aComposablePresenter withSpec: aSymbol
	
	^ self private_interpretASpec: (aComposablePresenter retrieveSpec: aSymbol) model: aComposablePresenter selector: aSymbol