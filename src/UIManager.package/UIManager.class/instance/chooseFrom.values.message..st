ui requests
chooseFrom: aList values: valueList message: messageString
	"Choose an item from the given list. Answer the index of the selected item."
	
	^self chooseFrom: aList values: valueList lines: #() message: messageString