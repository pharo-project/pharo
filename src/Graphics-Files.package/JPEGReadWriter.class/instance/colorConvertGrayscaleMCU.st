colorspace conversion
colorConvertGrayscaleMCU
	| ySampleStream y bits |
	ySampleStream := currentComponents at: 1.
	ySampleStream resetSampleStream.
	bits := mcuImageBuffer bits.
	1 
		to: bits size
		do: 
			[ :i | 
			y := ySampleStream nextSample + (residuals at: 2).
			y > MaxSample ifTrue: [ y := MaxSample ].
			residuals 
				at: 2
				put: (y bitAnd: ditherMask).
			y := y bitAnd: MaxSample - ditherMask.
			y < 1 ifTrue: [ y := 1 ].
			bits 
				at: i
				put: 4278190080 + (y << 16) + (y << 8) + y ]