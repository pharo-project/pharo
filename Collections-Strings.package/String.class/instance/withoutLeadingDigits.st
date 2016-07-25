converting
withoutLeadingDigits
	"Answer the portion of the receiver that follows any leading series of digits and separators.
	If the receiver consists entirely of digits and separators, return an empty string"

	^ self trimLeft: [ :char | char isDigit or: [ char isSeparator ] ]