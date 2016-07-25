accessing
string: aString

	string := aString.
	string isOctetString ifTrue: [ "Make sure that #hash will return the same value if the strings are equal."
		string := string asOctetString ].
	hash := string hash