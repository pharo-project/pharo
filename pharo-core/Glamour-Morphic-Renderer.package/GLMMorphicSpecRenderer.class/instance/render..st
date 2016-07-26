rendering
render: aPresentation
	container := GLMMorphic containerMorph.
	container
		changeTableLayout;
		listDirection: #leftToRight;
		vResizing: #spaceFill;
		hResizing: #spaceFill.
	container addMorphBack: aPresentation displayValue widget widget.
	aPresentation when: GLMPresentationUpdated send: #actOnPresentationUpdated: to: self.
	^ container