canvas drawing-rectangles
frameRectangle: aRectangle width: width colors: colors dashes: dashes
	| stroke |
	stroke := self canvas setStrokePaint: colors first.
	stroke width: width.
	stroke dashes: dashes offset: 0.
	self canvas drawShape: aRectangle.
	colors allButFirst
		ifNotEmpty: [ :e | 
			stroke fillPaint: e first.
			stroke dashes: dashes reverse offset: dashes first.
			self canvas drawShape: aRectangle ]