NOT CURRENTLY USED
fileDescriptorOpen: aFileDescription mode: aMode
	| cFile |
	cFile := self primitiveFdopen: aFileDescription mode: aMode.
	cFile isNull ifTrue: [ self perror: 'fdopen()' ].
	^ cFile
