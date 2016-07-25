ui requests
chooseFrom: labelList values: valueList title: aString
	"Choose an item from the given list. Answer the selected item."
	^self chooseFrom: labelList values: valueList lines: #() title: aString