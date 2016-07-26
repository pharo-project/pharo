instance creation
for: aRegex
	"Create and answer a matcher that will match a regular expression
	specified by the syntax tree of which `aRegex' is a root."

	^self for: aRegex ignoreCase: false