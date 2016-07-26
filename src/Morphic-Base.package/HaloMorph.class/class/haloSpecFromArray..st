halo theme
haloSpecFromArray: anArray 
	| aColor |
	aColor := Color.
	anArray fourth
		do: [:sel | aColor := aColor perform: sel].
	^ HaloSpec new
		horizontalPlacement: anArray second
		verticalPlacement: anArray third
		color: aColor
		iconSymbol: anArray fifth
		addHandleSelector: anArray first 