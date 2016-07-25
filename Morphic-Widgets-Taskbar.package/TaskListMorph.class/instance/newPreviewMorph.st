instance creation
newPreviewMorph
	"Answer a new preview holder."
	^ Morph new
		color: Color transparent;
		extent: self defaultPreviewExtent;
		yourself.