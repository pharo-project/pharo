instance creation
fromShortVersionString: aString
	"Create a new SystemVersion from the given short version string. Useful for testing."

	| parts |
	parts := self parseShortVersionString: aString.
	^self new
		type: parts first;
		major: parts second;
		minor: parts third;
		suffix: parts fourth;
		yourself