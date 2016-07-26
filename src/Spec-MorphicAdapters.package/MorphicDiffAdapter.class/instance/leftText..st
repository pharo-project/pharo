spec protocol
leftText: aText

	self widgetDo: [ :w | 
		w sourceTextModel setText: aText.
		w updateText ]