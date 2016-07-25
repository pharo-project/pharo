ui requests
chooseFrom: labelList values: valueList lines: linesArray title: aString
	"Choose an item from the given list. Answer the selected item."
	
	|modalMorph|
	modalMorph := self modalMorph.
	^modalMorph theme  
		chooseIn: modalMorph
		title: aString
		labels: labelList
		values: valueList
		lines: linesArray