pixel copies
copyPixelsGrayAlpha: y at: startX by: incX 
	"Handle interlaced grayscale with alpha color mode (colorType = 4)"
	| i pixel gray b |
	b := BitBlt bitPokerToForm: form.
	bitsPerChannel = 8 
		ifTrue: 
			[ startX 
				to: width - 1
				by: incX
				do: 
					[ :x | 
					i := (x // incX << 1) + 1.
					gray := thisScanline at: i.
					pixel := ((thisScanline at: i + 1) << 24) + (gray << 16) + (gray << 8) + gray.
					b 
						pixelAt: x @ y
						put: pixel ] ]
		ifFalse: 
			[ startX 
				to: width - 1
				by: incX
				do: 
					[ :x | 
					i := (x // incX << 2) + 1.
					gray := thisScanline at: i.
					pixel := ((thisScanline at: i + 2) << 24) + (gray << 16) + (gray << 8) + gray.
					b 
						pixelAt: x @ y
						put: pixel ] ]