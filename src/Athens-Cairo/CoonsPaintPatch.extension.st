Extension { #name : 'CoonsPaintPatch' }

{ #category : '*Athens-Cairo' }
CoonsPaintPatch >> addToCairoMeshGradientPaint: meshGradientPaint [

	meshGradientPaint
		beginPatch;
		moveTo: (self controlPoints at: 1);
		curveVia: (self controlPoints at: 2) and: (self controlPoints at: 3) to: (self controlPoints at: 4);
		curveVia: (self controlPoints at: 5) and: (self controlPoints at: 6) to: (self controlPoints at: 7);
		curveVia: (self controlPoints at: 8) and: (self controlPoints at: 9) to: (self controlPoints at: 10);
		curveVia: (self controlPoints at: 11) and: (self controlPoints at: 12) to: (self controlPoints at: 1);
		setCorner: 0 color: self colors first;
		setCorner: 1 color: self colors second;
		setCorner: 2 color: self colors third;
		setCorner: 3 color: self colors fourth;
		endPatch
]
