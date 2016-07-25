ui requests
chooseFrom: aList lines: linesArray message: messageString
	"Choose an item from the given list. Answer the index of the selected item."
	
	^self chooseFrom: aList lines: linesArray message: messageString title: ''