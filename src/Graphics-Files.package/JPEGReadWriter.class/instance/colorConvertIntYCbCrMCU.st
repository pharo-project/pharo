colorspace conversion
colorConvertIntYCbCrMCU
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
			cb := cbSampleStream nextSample - SampleOffset.
			cr := crSampleStream nextSample - SampleOffset.
			red := y + (FIXn1n40200 * cr // 65536) + (residuals at: 1).
			red > MaxSample 
				ifTrue: [ red := MaxSample ]
				ifFalse: [ red < 0 ifTrue: [ red := 0 ] ].
			residuals 
				at: 1
				put: (red bitAnd: ditherMask).
			red := red bitAnd: MaxSample - ditherMask.
			red < 1 ifTrue: [ red := 1 ].
			green := y - (FIXn0n34414 * cb // 65536) - (FIXn0n71414 * cr // 65536) + (residuals at: 2).
			green > MaxSample 
				ifTrue: [ green := MaxSample ]
				ifFalse: [ green < 0 ifTrue: [ green := 0 ] ].
			residuals 
				at: 2
				put: (green bitAnd: ditherMask).
			green := green bitAnd: MaxSample - ditherMask.
			green < 1 ifTrue: [ green := 1 ].
			blue := y + (FIXn1n77200 * cb // 65536) + (residuals at: 3).
			blue > MaxSample 
				ifTrue: [ blue := MaxSample ]
				ifFalse: [ blue < 0 ifTrue: [ blue := 0 ] ].
			residuals 
				at: 3
				put: (blue bitAnd: ditherMask).
			blue := blue bitAnd: MaxSample - ditherMask.
			blue < 1 ifTrue: [ blue := 1 ].
			bits 
				at: i
				put: 4278190080 + (red bitShift: 16) + (green bitShift: 8) + blue ]