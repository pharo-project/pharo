colorspace conversion
primColorConvertGrayscaleMCU
	self primColorConvertGrayscaleMCU: (currentComponents at: 1)
			bits: mcuImageBuffer bits
			residuals: residuals
			ditherMask: ditherMask.