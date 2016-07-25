services
enterOrRequestFrom: labelList values: valueList lines: linesArray title: aString
	"Choose an item from the given list. 
	 Answer the value selected of the selected item or the new string entered."
	
	^ self theme 
		enterOrRequestIn: self modalMorph
		title: aString
		labels: labelList
		values: valueList
		lines: linesArray