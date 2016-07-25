services
chooseFrom: aList lines: linesArray message: messageString title: aString
	"Choose an item from the given list. Answer the selected item."
	
	^(self
		chooseFrom: aList
		values: nil
		lines: linesArray
		message: messageString
		title: aString) ifNil: [0]