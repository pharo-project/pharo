instance creation
newImagePreviewMorph
	"Answer a new image preview morph."

	^ImagePreviewMorph new
		cornerStyle: self preferredCornerStyle;
		image: nil
		size: self previewSize