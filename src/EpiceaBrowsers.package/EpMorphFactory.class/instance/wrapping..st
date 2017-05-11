strings
wrapping: aMorphOrString

	^ Morph new
		changeTableLayout;
		color: Color transparent;
		addMorphBack: aMorphOrString asMorph;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		layoutInset: 2@0;
		yourself