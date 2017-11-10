Extension { #name : #ImageFillStyle }

{ #category : #'*Athens-Core' }
ImageFillStyle >> asAthensPaintOn: anAthensCanvas [

	^ (anAthensCanvas cacheAt: self ifAbsentPut: [
		anAthensCanvas surface
		createFormPaint: form ])
		origin: origin;
		direction: (form width@0)

]
