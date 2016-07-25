ui requests
chooseFrom: aList lines: linesArray message: messageString title: aString
	"Choose an item from the given list. Answer the selected item."
	
	^self
		chooseFrom: aList
		lines: linesArray
		title: (aString 
				ifEmpty: [messageString]
				ifNotEmpty: [aString, String cr, messageString])