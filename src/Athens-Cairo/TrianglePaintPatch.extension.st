Extension { #name : 'TrianglePaintPatch' }

{ #category : '*Athens-Cairo' }
TrianglePaintPatch >> addToCairoMeshGradientPaint: meshGradientPaint [

	meshGradientPaint
		beginPatch;
		moveTo: self controlPoints first;
		lineTo: self controlPoints second;
		lineTo: self controlPoints third;
		setCorner: 0 color: self colors first;
		setCorner: 1 color: self colors second;
		setCorner: 2 color: self colors third;
		endPatch
]
