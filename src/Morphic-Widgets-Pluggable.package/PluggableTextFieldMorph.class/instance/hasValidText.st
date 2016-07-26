testing
hasValidText
	"Return true if the text is valid with respect to the converter."

	|string|
	self converter ifNil: [^super hasValidText].
	string := self text asString.
	^(string isEmpty and: [self default notNil]) or: [
		self converter isStringValid: string]