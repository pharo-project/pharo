other
supportedDisplayDepths
	"Return all pixel depths supported on the current host platform."
	^#(1 2 4 8 16 32 -1 -2 -4 -8 -16 -32) select: [:d | self supportsDisplayDepth: d]