private
braceArray: anArray
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	collection := anArray.
	position := 0.
	readLimit := 0.
	writeLimit := anArray size.