ui requests
chooseFrom: aList title: aString
	"Choose an item from the given list. Answer the index of the selected item."
	^self chooseFrom: aList lines: #() title: aString