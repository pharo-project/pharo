rendering
renderContainerFor: aPane

	contentMorph := self 
		morph: (self renderObject: aPane presentations) 
		withTitleOrNilOf: aPane presentations.
		
	container
		addMorph: contentMorph fullFrame: (LayoutFrame identity);
		adoptPaneColor: Color transparent