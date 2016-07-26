colorspace conversion
colorConvertFloatYCbCrMCU
	| ySampleStream crSampleStream cbSampleStream y cb cr red green blue bits |
	ySampleStream := currentComponents at: 1.
	cbSampleStream := currentComponents at: 2.
	crSampleStream := currentComponents at: 3.
	ySampleStream resetSampleStream.
	cbSampleStream resetSampleStream.
	crSampleStream resetSampleStream.
	bits := mcuImageBuffer bits.
	1 
		to: bits size
		do: 
			[ :i | 
			y := ySampleStream nextSample.
			cb := cbSampleStream nextSample - FloatSampleOffset.
			cr := crSampleStream nextSample - FloatSampleOffset.
			red := self sampleFloatRangeLimit: y + (1.402 * cr).
			green := self sampleFloatRangeLimit: y - (0.34414 * cb) - (0.71414 * cr).
			blue := self sampleFloatRangeLimit: y + (1.772 * cb).
			bits 
				at: i
				put: 4278190080 + (red << 16) + (green << 8) + blue ]