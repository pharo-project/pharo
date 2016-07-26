initialize-release
beHexDigit

	| hexLetters |
	hexLetters := 'abcdefABCDEF'.
	predicate := [:char | char isDigit or: [hexLetters includes: char]].
	negation := [:char | char isDigit not and: [(hexLetters includes: char) not]]