instance creation
for: aRegex ignoreCase: aBoolean
	"Create and answer a matcher that will match a regular expression
	specified by the syntax tree of which `aRegex' is a root."

	^self new
		initialize: aRegex
		ignoreCase: aBoolean