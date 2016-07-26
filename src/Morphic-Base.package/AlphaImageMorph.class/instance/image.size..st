accessing
image: aForm size: aPoint
	"Set the image to be the form scaled to the given size and padded if neccesary."

	|f|
	"Convert color forms etc. to 32 bit before resizing since scaling of ColorForm introduces degraded color resolution.
	Most noticable with grayscale forms."
	(aForm depth < 32 and: [aForm depth > 4])
		ifTrue: [f := Form extent: aPoint depth: 32.
				f fillColor: (Color white alpha: 0.003922).
				f getCanvas translucentImage: aForm at: 0@0.
				f fixAlpha]
		ifFalse: [f := aForm].
	f := f scaledToSize: aPoint.
	self autoSize
		ifTrue: [super image: f]
		ifFalse: [image := f.
				self changed].
	self cachedForm: nil.
	self changed: #imageExtent