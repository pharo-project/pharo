Extension { #name : 'Point' }

{ #category : '*Morphic-Core' }
Point >> scaledByDisplayScaleFactor [

	^ self * WorldMorph displayScaleFactor
]
