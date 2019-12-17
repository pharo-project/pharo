Extension { #name : #CompiledMethod }

{ #category : #'*System-Sources' }
CompiledMethod >> putSource: sourceStr class: class category: catName withStamp: changeStamp priorMethod: priorMethod [
	^ self
		putSource: sourceStr
		withPreamble: [ :file | 
			class
				printCategoryChunk: catName
				on: file
				withStamp: changeStamp
				priorMethod: priorMethod.
			file cr ]
]

{ #category : #'*System-Sources' }
CompiledMethod >> putSource: source withPreamble: preambleBlock [
	"Store the source code for the receiver on an external file."

	SourceFiles 
		writeSource: source 
		preamble: (String streamContents: preambleBlock)
		onSuccess: [ :newSourcePointer :theChangesFileStream |
			"Method chunk needs a final ! !"
			(SourceChunkWriteStream on: theChangesFileStream) nextPut: ' '.
			"Update with new source pointer"
			self setSourcePointer: newSourcePointer ]
		onFail: [ 
			self becomeForward: (self copyWithSource: source) ]

]
