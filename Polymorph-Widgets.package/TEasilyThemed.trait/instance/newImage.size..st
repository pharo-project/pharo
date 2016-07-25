controls
newImage: aForm size: aPoint
	"Answer a new image."

	^self theme
		newImageIn: self
		form: aForm
		size: aPoint