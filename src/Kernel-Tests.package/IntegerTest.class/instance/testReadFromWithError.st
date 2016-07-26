tests - instance creation
testReadFromWithError
	"Ensure that a string that does not represent an integer raises an error."
	
	self should: [Integer readFrom: 'invalid'] raise: Error
