ui requests
chooseFrom: aList message: messageString title: aString
	"Choose an item from the given list. Answer the index of the selected item."
	
	^self chooseFrom: aList lines: #() message: messageString title: aString