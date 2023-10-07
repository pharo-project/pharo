Extension { #name : 'Color' }

{ #category : '*Random-Core' }
Color class >> random [
	"Return a random color that isn't too dark or under-saturated."

	| random |
	random := SharedRandom globalGenerator.
	^ self
		  h: 360.0 * random next
		  s: 0.3 + (random next * 0.7)
		  v: 0.4 + (random next * 0.6)
		  alpha: 1.0
]
