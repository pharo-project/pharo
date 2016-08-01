initialization
initialize
	super initialize.
	sideRuler := (Morph new
		color: Color yellow;
		extent: 50 @ 500;
		useGradientFill;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself) asSpecAdapter.
	currentVerticalScrollPosition := 0