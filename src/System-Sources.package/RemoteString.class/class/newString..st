instance creation
newString: aString
	"Answer an instance of me for string, aString, on changes file. 
	Put the string on the file and create the remote reference."

	^self new string: aString