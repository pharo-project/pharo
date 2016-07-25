private
glamorousNormalFillStyleFor: aMorph height: anInteger
	"Return the normal button fillStyle for the given button."
	
"	| baseColor |
	baseColor := self glamorousBaseColorFor: aMorph.
	^ self glamorousNormalFillStyleWithBaseColor: baseColor for: aMorph height: anInteger
"

	^ SolidFillStyle color: (self glamorousLightColorFor: aMorph)