Extension { #name : #Number }

{ #category : #'*Morphic-Core' }
Number >> scaledByDisplayScaleFactor [

	^ self * self currentWorld displayScaleFactor
]
