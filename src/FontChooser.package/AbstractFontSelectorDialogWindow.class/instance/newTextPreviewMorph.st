instance creation
newTextPreviewMorph
	"Answer a text entry morph for the preview of the font."

	^(self
		newTextEditorFor: self
		getText: #previewText 
		setText: nil
		getEnabled: nil)
		vResizing: #rigid;
		enabled: false;
		extent: 20@90