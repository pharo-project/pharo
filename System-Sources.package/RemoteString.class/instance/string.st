accessing
string 
	"Answer the receiver's string if remote files are enabled, or nil."

	^ SourceFiles 
		readStreamAtFileIndex: sourceFileNumber 
		atPosition: filePositionHi 
		ifPresent: [ :readStream | readStream nextChunk ]
		ifAbsent: [ nil ]	