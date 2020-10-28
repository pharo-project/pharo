Extension { #name : #CoonsPaintPatch }

{ #category : #'*Athens-Cairo' }
CoonsPaintPatch >> addToCairoMeshGradientPaint: meshGradientPaint [
	meshGradientPaint
		beginPatch;
		moveTo: (controlPoints at: 1);
		curveVia: (controlPoints at: 2) and: (controlPoints at: 3) to: (controlPoints at: 4);
		curveVia: (controlPoints at: 5) and: (controlPoints at: 6) to: (controlPoints at: 7);
		curveVia: (controlPoints at: 8) and: (controlPoints at: 9) to: (controlPoints at: 10);
		curveVia: (controlPoints at: 11) and: (controlPoints at: 12) to: (controlPoints at: 1);
		setCorner: 0 color: colors first;
		setCorner: 1 color: colors second;
		setCorner: 2 color: colors third;
		setCorner: 3 color: colors fourth;
		endPatch
]
