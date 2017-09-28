testing
validateClassName: aString
	"Validate if a string can be the name of a new class. Raise an error if not."
	
	self new builder name: aString.