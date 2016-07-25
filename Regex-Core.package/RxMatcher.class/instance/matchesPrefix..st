accessing
matchesPrefix: aString
	"Match against a string. Return true if a prefix matches.
	If you want to match 
		- the full string use #matches:
		- anywhere in the string use #search:"

	^self matchesStreamPrefix: aString readStream