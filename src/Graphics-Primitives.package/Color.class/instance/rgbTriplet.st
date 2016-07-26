other
rgbTriplet
	"Color fromUser rgbTriplet"

	^ Array
		with: (self red roundTo: 0.01)
		with: (self green roundTo: 0.01)
		with: (self blue roundTo: 0.01)
