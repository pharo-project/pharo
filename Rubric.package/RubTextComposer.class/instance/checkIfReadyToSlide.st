testing
checkIfReadyToSlide

	"Check whether we are now in sync with previously composed lines"

	(possibleSlide and: [currCharIndex > stopCharIndex]) ifFalse: [^self].

	[prevIndex < prevLines size
		and: [(prevLines at: prevIndex) first < (currCharIndex - deltaCharIndex)]]
			whileTrue: [prevIndex := prevIndex + 1].

	(prevLines at: prevIndex) first = (currCharIndex - deltaCharIndex) ifTrue: [
		"Yes -- next line will have same start as prior line."
		prevIndex := prevIndex - 1.
		possibleSlide := false.
		nowSliding := true
	] ifFalse: [
		prevIndex = prevLines size ifTrue: [
			"Weve reached the end of prevLines, so no use to keep looking for lines to slide."
			possibleSlide := false
		]
	]