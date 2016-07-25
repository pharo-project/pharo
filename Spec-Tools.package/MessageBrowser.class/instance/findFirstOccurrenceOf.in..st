text selection
findFirstOccurrenceOf: searchedString in: textToSearchIn
	"Return the first index of aString in textToSearchIn "
	| firstIndex |
	firstIndex := textToSearchIn findString: searchedString startingAt: 1.
	[ (firstIndex > 1) and: [ (textToSearchIn at: (firstIndex - 1)) isAlphaNumeric ] ]
		whileTrue: [
			firstIndex := textToSearchIn findString: searchedString startingAt: firstIndex +1 ].
		
	^ firstIndex