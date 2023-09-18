Extension { #name : #SolidFillStyle }

{ #category : #'*Athens-Balloon' }
SolidFillStyle >> asAthensPaintOn: anAthensCanvas [

	^ anAthensCanvas surface createSolidColorPaint: color
]
