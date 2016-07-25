initialization
initializeTransform
	transform := TransformMorph new.
	transform
		color: Color transparent;
		borderWidth: 0;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		disableTableLayout;
		bounds: super innerBounds.
	self addMorphBack: transform.
