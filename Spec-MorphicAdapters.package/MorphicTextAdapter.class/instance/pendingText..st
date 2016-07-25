spec protocol
pendingText: aText

	self widgetDo: [ :w | 
		w setText: aText.
		w hasUnacceptedEdits: true ]