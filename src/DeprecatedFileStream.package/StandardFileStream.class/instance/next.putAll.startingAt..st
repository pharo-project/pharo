accessing
next: anInteger putAll: aString startingAt: startIndex
	"Store the next anInteger elements from the given collection."
	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [
		position < readLimit ifTrue: [ self flushReadBuffer ] ].	
	self primWrite: fileID from: aString startingAt: startIndex count: anInteger.
	^aString