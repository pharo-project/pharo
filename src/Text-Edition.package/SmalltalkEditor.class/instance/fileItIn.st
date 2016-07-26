menu messages
fileItIn
	"Make a Stream on the text selection and fileIn it.
	 Moved here from FileController; this function can be useful from any text window that shows stuff in chunk format"

	| selection |
	selection := self selection.
	CodeImporter evaluateReadStream: (ReadWriteStream on: selection string from: 1 to: selection size).
	self inform: 'Code imported'.
