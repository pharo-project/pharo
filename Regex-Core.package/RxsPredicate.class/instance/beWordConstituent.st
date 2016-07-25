initialize-release
beWordConstituent

	predicate := [:char | char isAlphaNumeric or: [char == $_]].
	negation := [:char | char isAlphaNumeric not and: [char ~~ $_]]