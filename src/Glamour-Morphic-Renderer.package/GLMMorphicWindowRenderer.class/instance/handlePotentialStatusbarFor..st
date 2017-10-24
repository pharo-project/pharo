rendering
handlePotentialStatusbarFor: aRenderable
	| statusbarMorph bottomOffset |
	bottomOffset := 0.
	aRenderable hasStatusbar
		ifTrue: [ 
			statusbarMorph := GLMMorphicPaneWithoutTitleRenderer render: aRenderable statusbarPane from: self renderer.
			bottomOffset := (0 - (LabelMorph contents: 'XXX') minHeight) * 1.2.
			window
				addMorph: statusbarMorph
				fullFrame: (LayoutFrame identity 
					topFraction: 1; 
					leftOffset: 2;
					topOffset: bottomOffset;
					rightOffset: -2)].
	^ bottomOffset