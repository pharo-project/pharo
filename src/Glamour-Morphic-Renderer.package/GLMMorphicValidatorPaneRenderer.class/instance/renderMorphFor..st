rendering
renderMorphFor: aPane

	paneMorph
		addMorph: 
			("self newColumnWith:  {" (self renderObject: aPane presentations)".  (self buttonRowFrom: self buttons) }") 
			fullFrame: (LayoutFrame fractions: (0 @ 0 corner: 1 @ 0.90));
		addMorph:  (self buttonRowFrom: self buttons)
			fullFrame: (LayoutFrame fractions: (0 @ 0.93 corner: 1 @ 1));
		"adoptPaneColor: paneMorph paneColor"
		yourself