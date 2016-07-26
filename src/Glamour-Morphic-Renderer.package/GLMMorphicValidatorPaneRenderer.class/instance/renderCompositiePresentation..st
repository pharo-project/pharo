rendering
renderCompositiePresentation: aCompositePresentation
	
	paneMorph := GLMMorphic containerMorph.
	paneMorph
		addMorph: 
			(self newColumnWith:  { (self renderObject: aCompositePresentation).  (self buttonRowFrom: self buttons) }) 
			fullFrame: (LayoutFrame identity);
		"addMorphBack: (self buttonRowFrom: self buttons)  ;  "
		adoptPaneColor: paneMorph paneColor.
	
	^ paneMorph