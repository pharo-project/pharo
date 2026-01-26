Extension { #name : 'CompiledMethod' }

{ #category : '*System-Sources' }
CompiledMethod >> putSource: source withFooter: footer protocol: protocol timestamp: aTimestamp priorSourcePointer: priorSourcePointer [
	"Store the source code for the receiver on an external file."

	SourceFileArray default
		writeSource: source
		of: self
		withFooter: footer
		protocol: protocol
		timestamp: aTimestamp
		priorSourcePointer: priorSourcePointer
]
