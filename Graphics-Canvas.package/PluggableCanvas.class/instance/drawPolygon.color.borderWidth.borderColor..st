drawing-polygons
drawPolygon: vertices color: aColor borderWidth: bw borderColor: bc
	self apply: [ :c |
		c drawPolygon: vertices color: aColor borderWidth: bw borderColor: bc ]