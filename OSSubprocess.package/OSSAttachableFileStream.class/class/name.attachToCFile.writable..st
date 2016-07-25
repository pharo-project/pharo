TO USE LATER WITHOUT OSPROCESS
name: aSymbolOrString attachToCFile: aCFile writable: readWriteFlag 
	"Create a new instance attached to aCFile. For write streams, this represents two
	Smalltalk streams which write to the same OS file or output stream,
	presumably with interleaved output. The purpose of this method is to
	permit a FileStream to be attached to an existing aCFile, such as
	the handle for standard input, standard output, and standard error."

	^ (super basicNew
		name: aSymbolOrString
		attachToCFile: aCFile
		writable: readWriteFlag) initialize