initialize - release
changeString: aString
	"Install aString as the one to be edited by the receiver."

	string := aString.
	self resetState