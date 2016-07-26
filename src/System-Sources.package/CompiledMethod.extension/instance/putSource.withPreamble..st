*System-Sources
putSource: source withPreamble: preambleBlock
	"Store the source code for the receiver on an external file."

	SourceFiles 
		writeSource: source 
		preamble: (String streamContents: preambleBlock)
		onSuccess: [ :newSourcePointer | 
			"Method chunk needs a final ! !"
			SourceFiles changesWriteStreamDo: [ :file | file nextChunkPut: ' ' ].
			"Update with new source pointer"
			self setSourcePointer: newSourcePointer ]
		onFail: [ 
			self becomeForward: (self copyWithSource: source) ]
