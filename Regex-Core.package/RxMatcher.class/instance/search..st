accessing
search: aString
	"Search anywhere in the String for occurrence of something matching myself.
	If you want to match the full String see #matches:
	Answer a Boolean indicating success."

	^self searchStream: aString readStream