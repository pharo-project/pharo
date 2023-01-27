Extension { #name : #GenericPaintPatch }

{ #category : #'*Athens-Cairo' }
GenericPaintPatch >> addToCairoMeshGradientPaint: meshGradientPaint [
	meshGradientPaint beginPatch.
	[
		buildBlock value: meshGradientPaint
	] ensure: [ meshGradientPaint endPatch ]
]
