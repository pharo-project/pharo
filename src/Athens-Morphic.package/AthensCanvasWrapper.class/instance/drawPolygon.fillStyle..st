canvas drawing-polygons
drawPolygon: vertices fillStyle: aFillStyle
	| path |
	self canvas pathTransform
		restoreAfter: [ 
			self canvas pathTransform.
			path := self createClosedPolygonPathFrom: vertices ].
	self canvas setPaint: aFillStyle.
	self canvas drawShape: path