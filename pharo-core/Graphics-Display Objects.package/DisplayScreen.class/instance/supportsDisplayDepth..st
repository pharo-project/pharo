other
supportsDisplayDepth: pixelDepth
	"Return true if this pixel depth is supported on the current host platform.
	Primitive. Optional."
	<primitive: 91>
	^#(1 2 4 8 16 32) includes: pixelDepth