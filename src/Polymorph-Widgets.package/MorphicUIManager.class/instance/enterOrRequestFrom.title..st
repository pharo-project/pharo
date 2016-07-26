ui requests
enterOrRequestFrom: aList title: aString
	"Choose an item from the given list. Answer the index of the selected item."
	^self enterOrRequestFrom: aList lines: #() title: aString