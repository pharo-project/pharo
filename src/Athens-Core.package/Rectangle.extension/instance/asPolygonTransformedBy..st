*Athens-Core
asPolygonTransformedBy: aTransform
	"answer a clock-wise oriented polygon, representing receiver,
	transformed by transform"
	
	^ {
		aTransform transform: origin.
		aTransform transformX: corner x Y: origin y.
		aTransform transform: corner.
		aTransform transformX: origin x Y: corner y.
	}