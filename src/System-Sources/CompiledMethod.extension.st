Extension { #name : 'CompiledMethod' }

{ #category : '*System-Sources' }
CompiledMethod >> putSource: sourceStr class: class protocol: protocol withStamp: changeStamp priorMethod: priorMethod [

	^ self putSource: sourceStr withPreamble: [ :file |
		  class
			  printProtocolChunk: protocol
			  on: file
			  withStamp: changeStamp
			  priorMethod: priorMethod.
		  file cr ]
]

{ #category : '*System-Sources' }
CompiledMethod >> putSource: source withPreamble: preambleBlock [
	"Store the source code for the receiver on an external file."

	SourceFiles
		writeSource: source
		preamble: (String streamContents: preambleBlock)
		onSuccess: [ :newSourcePointer :theChangesFileStream |
			"Method chunk needs a final ! !"
			(SourceChunkWriteStream on: theChangesFileStream) nextPut: ' '.
			"Update with new source pointer"
			self sourcePointer: newSourcePointer ]
		onFail: [
			"if we can not store the source pointer, we put back the property"
			self propertyAt: #source put: source ]
]
