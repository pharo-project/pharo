Extension { #name : #SolidFillStyle }

{ #category : #'*Athens-Balloon' }
SolidFillStyle >> = aFillStyle [

	^ aFillStyle species = self species
		and: [ self asColor = aFillStyle asColor ]
]

{ #category : #'*Athens-Balloon' }
SolidFillStyle >> asAthensPaintOn: anAthensCanvas [ 

	^ anAthensCanvas surface createSolidColorPaint: color
]

{ #category : #'*Athens-Balloon' }
SolidFillStyle >> hash [

	^ color hash
]
