accessing
image: anImage 
	"Fixed to take account of border width. Use narrowest
	dimanesion of image to allow a little flexibility."
	
	image := anImage depth = 1 
				ifTrue: [ColorForm mappingWhiteToTransparentFrom: anImage]
				ifFalse: [anImage]. 
	self extent: 2 * self borderWidth + image extent min asPoint.
	self changed