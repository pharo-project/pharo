accessing
setLabelWidgetAllowance
	^ labelWidgetAllowance :=  super setLabelWidgetAllowance +
		(toolbarBox
			ifNil: [ 0 ]
			ifNotNil: [ :toolbar | toolbarBox width ])