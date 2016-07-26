filtering
filterAverage: count 
	"Use the average of the pixel to the left and the pixel above as a predictor"
	| delta |
	delta := bitsPerPixel // 8 max: 1.
	1 
		to: delta
		do: 
			[ :i | 
			thisScanline 
				at: i
				put: ((thisScanline at: i) + ((prevScanline at: i) // 2) bitAnd: 255) ].
	delta + 1 
		to: count
		do: 
			[ :i | 
			thisScanline 
				at: i
				put: ((thisScanline at: i) + (((prevScanline at: i) + (thisScanline at: i - delta)) // 2) bitAnd: 255) ]