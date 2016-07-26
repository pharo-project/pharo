primitives-incremental
primRenderImage: edge with: fill
	"Start/Proceed rendering the current scan line"
	<primitive: 'primitiveRenderImage' module: 'B2DPlugin'>
	^self primitiveFailed