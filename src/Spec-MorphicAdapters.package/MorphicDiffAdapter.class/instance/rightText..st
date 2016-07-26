spec protocol
rightText: aText
	self
		widgetDo: [ :w | 
			w destTextModel setText: aText.
			w updateText ]