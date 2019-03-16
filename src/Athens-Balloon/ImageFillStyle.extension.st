Extension { #name : #ImageFillStyle }

{ #category : #'*Athens-Balloon' }
ImageFillStyle >> asAthensPaintOn: anAthensCanvas [

	^ (anAthensCanvas cacheAt: self ifAbsentPut: [
		anAthensCanvas surface
		createFormPaint: form ])
		origin: origin
]
