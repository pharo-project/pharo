testing
hasSource
	^ kind == #EmbeddedSource or: [ kind == #EmbeddedSourceQCompress or: [ kind == #EmbeddedSourceZip ] ]