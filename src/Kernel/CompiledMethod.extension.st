Extension { #name : 'CompiledMethod' }

{ #category : '*Kernel' }
CompiledMethod >> putSource: source withFooter: footer protocol: protocol timestamp: aTimestamp priorSourcePointer: priorSourcePointer [
	"Store the source code for the receiver on an external file."

	SourcesManager current
		writeSource: source
		of: self
		withFooter: footer
		protocol: protocol
		timestamp: aTimestamp
		priorSourcePointer: priorSourcePointer
]
