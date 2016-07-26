accessing
contents: anObject

	"Unconditionally reset the contents, including selection and undo/redo buffers"
	
	self text: anObject asTxModel.
