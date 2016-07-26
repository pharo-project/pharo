conversion
isStringValid: aString
	"Answer whether the given string is valid for conversion."

	^(self regex isNil or: [self regex matches: aString]) and: [
		[self stringAsObject: aString. true] on: Error do: [false]]