ui requests
chooseOrRequestFrom: aList title: aString
	"Choose an item from the given list. Answer the index of the selected item."
	^self chooseOrRequestFrom: aList lines: #() title: aString