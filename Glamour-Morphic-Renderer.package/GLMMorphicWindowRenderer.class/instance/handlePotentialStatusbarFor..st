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
				fullFrame: (LayoutFrame fractions: (0 @ 1 corner: 1 @ 1) offsets: (2 @ bottomOffset corner: -2 @ 0)) ].
	^ bottomOffset