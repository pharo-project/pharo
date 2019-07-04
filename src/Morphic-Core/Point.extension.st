Extension { #name : #Point }

{ #category : #'*Morphic-Core' }
Point >> scaledByDisplayScaleFactor [

	^ self * self currentWorld displayScaleFactor.
]
