token testing
isName
	^ (currentTokenFirst isLetter
			or: [currentTokenFirst == $_])
		and: [currentToken last isAlphaNumeric
				or: [currentToken last == $_]]