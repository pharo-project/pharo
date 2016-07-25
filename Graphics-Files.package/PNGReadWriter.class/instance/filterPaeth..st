filtering
filterPaeth: count 
	"Select one of (the pixel to the left, the pixel above and the pixel to above left) to
	predict the value of this pixel"
	| delta |
	delta := bitsPerPixel // 8 max: 1.
	1 
		to: delta
		do: 
			[ :i | 
			thisScanline 
				at: i
				put: ((thisScanline at: i) + (prevScanline at: i) bitAnd: 255) ].
	delta + 1 
		to: count
		do: 
			[ :i | 
			thisScanline 
				at: i
				put: ((thisScanline at: i) + (self 
						paethPredictLeft: (thisScanline at: i - delta)
						above: (prevScanline at: i)
						aboveLeft: (prevScanline at: i - delta)) bitAnd: 255) ]