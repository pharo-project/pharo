private
stringIndex: anInteger text: aText topLeft: topLeft extent: extent 
	stringIndex := anInteger.
	text := aText.
	super 
		setPoint: topLeft
		point: topLeft + extent