public
readUpToEndOfStyleChunk
	"Style chunks are between $] and $["
	^decoratedStream upTo: $[