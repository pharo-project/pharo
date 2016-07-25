filtering
filterVertical: count
	"Use the pixel above as a predictor"

	1 to: count do: [ :i |
		thisScanline at: i put: (((thisScanline at: i) +
(prevScanline at: i)) bitAnd: 255) ]

