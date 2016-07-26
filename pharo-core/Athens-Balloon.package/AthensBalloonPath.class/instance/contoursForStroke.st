converting
contoursForStroke
	"answer a collection of contours, ready for stroking by balloon engine"
	^ contours collect: [:contour | contour asArray ]