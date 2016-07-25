services
enterOrRequestFrom: aList lines: linesArray title: aString
	"Choose an item from the given list. 
	 Answer the value selected of the selected item or the new string entered."
	
	^self
		enterOrRequestFrom: aList
		values: aList
		lines: linesArray
		title: aString