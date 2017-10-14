tests - input
testReadFromWithError
	"self debug: #testReadFromWithError"
	
	| string |
	string := 'invalid'.
	self should: [self timeClass readFrom: string readStream] raise: Error.
	string := '0:invalid'.
	self should: [self timeClass readFrom: string readStream] raise: Error
