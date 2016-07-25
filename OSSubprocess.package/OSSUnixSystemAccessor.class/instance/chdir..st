cwd
chdir: aDirString
	"Changes the current working directory of the vmProcess to aDirString"
	| returnValue | 
	returnValue := self primitiveChdir: aDirString.
	(returnValue = -1) ifTrue: [ self perror: 'chdir()' ].
	(returnValue = 0) ifFalse: [ self error: 'Unexpected return value from chdir() ', returnValue printString ].
	