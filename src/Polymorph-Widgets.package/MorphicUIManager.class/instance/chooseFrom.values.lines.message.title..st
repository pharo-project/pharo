services
chooseFrom: labelList values: valueList lines: linesArray message: messageString title: aString
	"Choose an item from the given list. Answer the selected item."
	
	^ self theme
		chooseIn: self modalMorph
		title: aString
		message: messageString
		labels: labelList
		values: valueList
		lines: linesArray