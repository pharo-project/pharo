rendering
renderMorphFor: aPane

	paneMorph
		addMorph: 
			("self newColumnWith:  {" (self renderObject: aPane presentations)".  (self buttonRowFrom: self buttons) }") 
			fullFrame: (LayoutFrame identity bottomFraction: 0.90);
		addMorph:  (self buttonRowFrom: self buttons)
			fullFrame: (LayoutFrame identity topFraction: 0.93)
		"adoptPaneColor: paneMorph paneColor"
		yourself