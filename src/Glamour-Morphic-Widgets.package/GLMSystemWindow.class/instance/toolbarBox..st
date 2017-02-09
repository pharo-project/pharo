accessing
toolbarBox: anObject
	| previousToolbar |
	
	previousToolbar := toolbarBox ifNil: [ menuBox ].
	
	toolbarBox := anObject.
	self setLabelWidgetAllowance.
	self replacePane: previousToolbar with: toolbarBox