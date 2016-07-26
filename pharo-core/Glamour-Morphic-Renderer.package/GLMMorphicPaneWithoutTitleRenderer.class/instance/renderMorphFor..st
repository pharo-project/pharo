rendering
renderMorphFor: aPane
	paneMorph
		addMorph: (self renderObject: aPane presentations) fullFrame: (LayoutFrame identity);
		adoptPaneColor: paneMorph paneColor