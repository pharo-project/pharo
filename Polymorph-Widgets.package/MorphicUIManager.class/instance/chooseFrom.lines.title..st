ui requests
chooseFrom: aList lines: linesArray title: aString
	"Choose an item from the given list. Answer the index of the selected item."
	
	^(self
		chooseFrom: aList
		values: nil
		lines: linesArray
		title: aString) ifNil: [0]