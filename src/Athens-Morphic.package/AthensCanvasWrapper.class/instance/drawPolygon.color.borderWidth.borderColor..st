canvas drawing-polygons
drawPolygon: vertices color: aColor borderWidth: aBorderWidth borderColor: aBorderColor
	| path |
	self canvas pathTransform
		restoreAfter: [ 
			self canvas pathTransform.
			path := self createClosedPolygonPathFrom: vertices ].
	self canvas setPaint: aColor.
	self canvas drawShape: path.
	(self canvas setStrokePaint: aBorderColor) width: aBorderWidth.
	self canvas drawShape: path