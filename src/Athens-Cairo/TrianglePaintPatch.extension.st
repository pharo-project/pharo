Extension { #name : #TrianglePaintPatch }

{ #category : #'*Athens-Cairo' }
TrianglePaintPatch >> addToCairoMeshGradientPaint: meshGradientPaint [
	meshGradientPaint
		beginPatch;
		moveTo: controlPoints first;
		lineTo: controlPoints second;
		lineTo: controlPoints third;
		setCorner: 0 color: colors first;
		setCorner: 1 color: colors second;
		setCorner: 2 color: colors third;
		endPatch
]
