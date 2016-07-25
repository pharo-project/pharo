*Spec-MorphicAdapters
treeRenderOn: aCanvas bounds: drawBounds color: drawColor font: aFont from: aMorph

	self bounds: drawBounds.
	aMorph addMorphBack: self.
       