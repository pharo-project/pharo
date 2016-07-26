colorspace conversion
colorConvertMCU

	^ currentComponents size = 3
		ifTrue:
			[self useFloatingPoint
				ifTrue: [self colorConvertFloatYCbCrMCU]
				ifFalse: [self primColorConvertYCbCrMCU: currentComponents
								bits: mcuImageBuffer bits
								residuals: residuals
								ditherMask: ditherMask.]]
		ifFalse: [self primColorConvertGrayscaleMCU]