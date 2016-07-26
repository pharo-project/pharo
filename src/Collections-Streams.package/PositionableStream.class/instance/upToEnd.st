accessing
upToEnd
	"Answer a subcollection from the current access position through the last element of the receiver."

	| newStream |
	newStream := (collection species new: 100) writeStream.
	[self atEnd] whileFalse: [ newStream nextPut: self next ].
	^ newStream contents