accessing
upToEnd
	"Answer a subcollection from the current access position through the last element of the receiver."

	| newStream buffer |
	buffer := collection species new: 1000.
	newStream := (collection species new: 100) writeStream.
	[self atEnd] whileFalse: [newStream nextPutAll: (self nextInto: buffer)].
	^ newStream contents