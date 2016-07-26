colorspace conversion
primColorConvertYCbCrMCU: componentArray bits: bits residuals: residualArray ditherMask: mask
	<primitive: 'primitiveColorConvertMCU' module: 'JPEGReaderPlugin'>
	^self colorConvertIntYCbCrMCU