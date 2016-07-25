instance creation
block: aBlock layoutFrame: aLayoutFrame

	| row |
	row := SpecColumnLayout composed.
	aBlock value: row.
	subwidget := row asArray.
	self layoutFrame: aLayoutFrame.
	self generateArguments