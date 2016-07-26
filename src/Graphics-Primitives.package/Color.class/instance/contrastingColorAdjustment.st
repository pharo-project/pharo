conversions
contrastingColorAdjustment
	"Answer darker or lighter color depending on the luminance."

	self isTransparent ifTrue: [ ^ self ].
	
	^ self luminance > 0.5
		ifTrue: [ self darker ] 
		ifFalse:  [ self lighter ]