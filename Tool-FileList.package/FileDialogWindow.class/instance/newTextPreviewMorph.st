instance creation
newTextPreviewMorph
	"Answer a new text preview morph."

	^(self newTextEditorFor: self getText: nil setText: nil getEnabled: nil)
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		extent: self previewSize;
		minWidth: self previewSize x;
		minHeight: self previewSize y;
		enabled: false