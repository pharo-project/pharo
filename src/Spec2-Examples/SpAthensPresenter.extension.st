Extension { #name : #SpAthensPresenter }

{ #category : #'*Spec2-Examples' }
SpAthensPresenter class >> example [
	| extent |
	
	extent := 350@300.
	self new
		surfaceExtent: extent;
		drawBlock: [ :aCanvas | | paint surface |
			surface := aCanvas surface.
			paint := surface 
				createLinearGradient: { 
					0->Color red. 
					1->Color green } 
				start: 0@0 
				stop: extent.
			surface clear.
			aCanvas setPaint: paint.
			aCanvas drawShape: (0@0 corner: extent) ]; 
		openWithSpec
]
