filtering
filterHorizontal: count 
	"Use the pixel to the left as a predictor"
	| delta |
	delta := bitsPerPixel // 8 max: 1.
	delta + 1 
		to: count
		do: 
			[ :i | 
			thisScanline 
				at: i
				put: ((thisScanline at: i) + (thisScanline at: i - delta) bitAnd: 255) ]