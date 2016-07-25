accessing
matches: aString
	"Match against a string. Return true if the complete String matches.
	If you want to search for occurences anywhere in the String see #search:"

	^self matchesStream: aString readStream