brick-properties
textColor: aColor
	<brickBuilderProperty: #Text name: 'Text color' as: #brickBuilderEvaluate getter: #textColor model: #GLMUIBuilderTextFieldProperty>

	self label textColor: aColor.
	self dotsBrick textColor: aColor