settings
createMissingStandardStreams: aBoolean
	"If we don't create streams for the standard streams of the child process,
	then the child will be used the inherit standard files from the parent. 
	
	If aBoolean is true, then we will automatically create streams for mapping stdout/stderr/stdin.
	Note that we will create streams ONLY for those standard streams that had not already been set by
	another parts of the API (like #redirectStdoutTo:, or #createAndSetStdeoutStream, etc.).
	In addition, the type of streams that we will automatically create is defined by defaultReadStreamCreationBlock
	and defaultWriteStreamCreationBlock. So you can customize that to define the type of stream automatically created. 
	
	"
	createMissingStandardStreams := aBoolean
	