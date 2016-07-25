files
fileno: aCFile
	"Answers the file descriptor associated to the FILE* represented by aCFile"
	| returnValue |
	returnValue := self primitiveFileno: aCFile.
	(returnValue = -1) ifTrue: [ self perror: 'fileno()' ].
	^ returnValue "file descriptor"