primitives-incremental
primRenderScanline: edge with: fill
	"Start/Proceed rendering the current scan line"
	<primitive: 'primitiveRenderScanline' module: 'B2DPlugin'>
	^self primitiveFailed