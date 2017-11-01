Extension { #name : #MorphicEvent }

{ #category : #'*Glamour-Morphic-Brick' }
MorphicEvent >> withHandPositionDo: aBlock [
	"Compatibility method to make Spotter work with Bloc."
	aBlock value: self position
]
