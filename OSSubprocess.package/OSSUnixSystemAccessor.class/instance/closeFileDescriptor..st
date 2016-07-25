files
closeFileDescriptor: aFileDescription
	"It closes the stream associated to aFileDescriptor"
	| returnValue |
	returnValue := self primitiveClose: aFileDescription.
	(returnValue = -1) ifTrue:  [ self perror: 'close()' ].
	^ returnValue
