actions
updatePointSize

	pointSizeList ifNotNil: [ pointSizeList selectionIndex: model selectedPointSizeIndex ].
	"this should not be like that but managed by the list!!!!"
	
	self updatePreview