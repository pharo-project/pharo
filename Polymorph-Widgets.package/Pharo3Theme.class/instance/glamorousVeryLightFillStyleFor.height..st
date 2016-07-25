private
glamorousVeryLightFillStyleFor: aMorph height: anInteger
	"Return the very light button fillStyle for the given button."
	
"	| baseColor |
	baseColor := self glamorousBaseColorFor: aMorph.
	^ self glamorousNormalFillStyleWithBaseColor: baseColor for: aMorph height: anInteger
"
	^ SolidFillStyle color: (self glamorousVeyLightSelectionColorFor: aMorph)