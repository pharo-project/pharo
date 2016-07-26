colorspace conversion
primColorConvertIntYCbCrMCU
	self primColorConvertYCbCrMCU: currentComponents
			bits: mcuImageBuffer bits
			residuals: residuals
			ditherMask: ditherMask.