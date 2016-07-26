accessing
colorsFromArray: colorArray
	| colorList |
	colorList := colorArray collect: [:colorDef |
		Color fromArray: colorDef].
	self colors: colorList