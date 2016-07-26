primitives-incremental
primMergeFill: fillBitmap from: fill
	"Merge the filled bitmap into the current output buffer."
	<primitive: 'primitiveMergeFillFrom' module: 'B2DPlugin'>
	^self primitiveFailed