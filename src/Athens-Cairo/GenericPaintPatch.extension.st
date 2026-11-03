Extension { #name : 'GenericPaintPatch' }

{ #category : '*Athens-Cairo' }
GenericPaintPatch >> addToCairoMeshGradientPaint: meshGradientPaint [

	meshGradientPaint beginPatch.
	[ self buildBlock value: meshGradientPaint ] ensure: [ meshGradientPaint endPatch ]
]
