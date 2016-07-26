converting
withoutTrailingDigits
	"Answer the portion of the receiver that precedes any leading series of digits and separators.
	If the receiver consists entirely of digits and separators, return an empty string"

	^ self trimRight: [ :char | char isDigit or: [ char isSeparator ] ]