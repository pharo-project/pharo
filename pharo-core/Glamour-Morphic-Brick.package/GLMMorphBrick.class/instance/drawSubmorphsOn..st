morph-drawing
drawSubmorphsOn: aCanvas
	aCanvas
		translateBy: self bounds origin - self morph bounds origin + (self paddingLeft @ self paddingTop)
		during: [ :canvas | self morph fullDrawOn: canvas ]