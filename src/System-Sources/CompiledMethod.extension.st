Extension { #name : 'CompiledMethod' }

{ #category : '*System-Sources' }
CompiledMethod >> putSource: source withFooter: footer protocol: protocol timestamp: aTimestamp priorSourcePointer: priorSourcePointer [
	"Store the source code for the receiver on an external file."

	SourceFileArray default newWriter
		metadata: footer;
		timestamp: aTimestamp;
		prior: priorSourcePointer;
		successBlock: [ :newSourcePointer |
			"Update with new source pointer"
			self sourcePointer: newSourcePointer ];
		failBlock: [
			"if we can not store the source pointer, we put back the property"
			self propertyAt: #source put: source ];
		writeSource: source ofMethod: self protocol: protocol
]
