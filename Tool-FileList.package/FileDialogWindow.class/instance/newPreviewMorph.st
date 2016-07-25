instance creation
newPreviewMorph
	"Answer a new preview morph."

	self previewType == #image ifTrue: [^self newImagePreviewMorph].
	self previewType == #text ifTrue: [^self newTextPreviewMorph].
	^nil