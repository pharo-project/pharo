transformations
duller
	"Answer a darker, desaturated color.  If the original color isn't very saturated, desaturate it by less (otherwise will just end up with grey)."
	
	| sat adjust |
	(sat := self saturation) > 0.3
		ifTrue: [adjust := -0.1]
		ifFalse: [adjust := 0.1 - sat max: 0.0].
	^ self adjustSaturation: adjust brightness: -0.1

	"^ self adjustSaturation: -0.03 brightness: -0.2"