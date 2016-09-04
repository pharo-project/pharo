accessing
nextPutAll: stringOrByteArray
	"Write all the characters of the given string to this file."

	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	self primWrite: fileID from: stringOrByteArray startingAt: 1 count: stringOrByteArray basicSize.
	^ stringOrByteArray
