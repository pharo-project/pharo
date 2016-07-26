instance creation
block: aBlock layoutFrame: aLayoutFrame

	| row |
	row := SpecRowLayout composed.
	aBlock value: row.
	subwidget := row asArray.
	self layoutFrame: aLayoutFrame.
	self generateArguments