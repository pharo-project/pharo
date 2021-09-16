Extension { #name : #Point }

{ #category : #'*Deprecated10' }
Point >> dist: aPoint [ 
	"Answer the distance between aPoint and the receiver."
	
	self
		deprecated: 'Use #distanceTo: instead.'
		on: '9 February 2021'
		in: #Pharo9
		transformWith: '`@receiver dist: `@arg' -> '`@receiver distanceTo: `@arg'.
	^ self distanceTo: aPoint
]
