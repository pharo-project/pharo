initialize-release
substring: aString ignoreCase: aBoolean
	"Match exactly this string."

	sample := aString.
	aBoolean ifTrue: [self beCaseInsensitive]