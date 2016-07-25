actions
updateFontIndex

	fontList ifNotNil: [ fontList selectionIndex: model selectedFontIndex ].
	styleList ifNotNil: [ styleList updateList ].
	pointSizeList ifNotNil: [ pointSizeList updateList ].
	self updatePreview