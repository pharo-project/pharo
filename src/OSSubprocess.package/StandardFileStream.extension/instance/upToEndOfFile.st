*OSSubprocess
upToEndOfFile
	"Answer a subcollection from the current access position through the last element of the receiver.  Use #atEndOfFile to determine end of file status with feof(), required for reliable end of file test on OS pipes or in files that could have been written by another (child) process different than the VM process."
	

	| newStream buffer |
	self closed 
		ifTrue: [ self error: 'File already closed' ]
		ifFalse: [  
			buffer := buffer1 species new: 1000.
			newStream := WriteStream on: (buffer1 species new: 100).
			[self atEndOfFile] whileFalse: [newStream nextPutAll: (self nextInto: buffer)].
			^ newStream contents
		]