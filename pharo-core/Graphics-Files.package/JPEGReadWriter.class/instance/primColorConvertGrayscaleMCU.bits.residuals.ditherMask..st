colorspace conversion
primColorConvertGrayscaleMCU: componentArray bits: bits residuals: residualArray ditherMask: mask
	<primitive: 'primitiveColorConvertGrayscaleMCU' module: 'JPEGReaderPlugin'>
	"JPEGReaderPlugin doPrimitive: #primitiveColorConvertGrayscaleMCU."
	^self colorConvertGrayscaleMCU