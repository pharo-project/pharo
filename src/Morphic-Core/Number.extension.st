Extension { #name : 'Number' }

{ #category : '*Morphic-Core' }
Number >> scaledByDisplayScaleFactor [

	^ self * WorldMorph displayScaleFactor
]
