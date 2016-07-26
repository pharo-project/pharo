accessing
compressedSourceCode
	"Answer the source code of compiled method.
	Note: it does not attempts to read from source files using sourcePointer,
	nor reconstruct the source code using temp names"
	
	(kind == #EmbeddedSourceQCompress or: [ kind == #EmbeddedSourceZip ]) ifTrue: [
		^ data ].

	^ nil