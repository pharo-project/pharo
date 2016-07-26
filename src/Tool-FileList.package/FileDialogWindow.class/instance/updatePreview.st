actions
updatePreview
	"Update the preview."

	self previewType == #image ifTrue: [self updateImagePreview].
	self previewType == #text ifTrue: [self updateTextPreview]