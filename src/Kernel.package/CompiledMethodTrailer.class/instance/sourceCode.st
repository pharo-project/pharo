accessing
sourceCode
	"Answer the source code of compiled method as it was decoded.
	Note: it does not attempts to read from source files using sourcePointer,
	nor reconstruct the source code using temp names"
	
	(kind == #EmbeddedSource or: [kind == #EmbeddedSourceQCompress or: [ kind == #EmbeddedSourceZip ]]) ifTrue: [
		^ data ].

	^ nil