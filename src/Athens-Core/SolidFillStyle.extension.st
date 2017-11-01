Extension { #name : #SolidFillStyle }

{ #category : #'*Athens-Core' }
SolidFillStyle >> asAthensPaintOn: anAthensCanvas [ 

	^ anAthensCanvas surface createSolidColorPaint: color
]
