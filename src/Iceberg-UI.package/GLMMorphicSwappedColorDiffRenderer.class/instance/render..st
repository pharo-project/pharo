rendering
render: aPresentation
	| textMorph |
	textMorph := (SwappedColorDiffMorph
		from: aPresentation displayValue first
		to: aPresentation displayValue last
		contextClass: aPresentation contextClassValue)
			layoutFrame: (LayoutFrame identity);
			vResizing: #spaceFill;
			hResizing: #spaceFill;
			yourself.
	^textMorph