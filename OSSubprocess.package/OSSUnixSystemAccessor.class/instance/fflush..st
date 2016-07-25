files
fflush: aCFile
	"Flushes the file associated to the FILE* represented by aCFile"
	| returnValue |
	aCFile ifNil: [ self error: 'We disallow the fflush() of nil files since it will flush all open output streams' ].
	returnValue := self primitiveFflush: aCFile.
	(returnValue = 0) ifFalse: [ self perror: 'fflush()' ].
	