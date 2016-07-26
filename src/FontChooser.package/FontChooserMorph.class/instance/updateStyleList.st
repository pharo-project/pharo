actions
updateStyleList

	styleList ifNotNil: [ styleList selectionIndex: model selectedFontStyleIndex ].
	"this should not be like that but managed by the list!!!!"
	
	self updatePreview