brace support
braceStream: nElements
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	^ WriteStream basicNew braceArray: (self new: nElements)
