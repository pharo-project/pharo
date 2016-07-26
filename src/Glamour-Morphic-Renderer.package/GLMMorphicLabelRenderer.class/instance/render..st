rendering
render: aLabelPresentation
	labelMorph := (LabelMorph contents: aLabelPresentation formatedDisplayValue)
		layoutFrame: (LayoutFrame identity);
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		font: StandardFonts haloFont;
		yourself.
	aLabelPresentation when: GLMPresentationUpdated send: #actOnPresentationUpdated: to: self.
	^ labelMorph