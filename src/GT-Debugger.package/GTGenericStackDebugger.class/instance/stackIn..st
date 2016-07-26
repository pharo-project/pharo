building presentations
stackIn: composite

	^ (self stackWidgetIn: composite)
		with: [ :stackWidget | self installStackDebuggingActionsFor: stackWidget ]