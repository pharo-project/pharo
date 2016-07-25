display
drawForForm: aForm on: aCanvas
	"Draw a small view of the given form on the canvas"

	| scale shrunkForm viewedObjectBox interimCanvas |
	viewedObjectBox := aForm boundingBox.
	scale :=  self scaleFor: viewedObjectBox in: self innerBounds.
	interimCanvas := Display defaultCanvasClass extent: viewedObjectBox extent depth: aCanvas depth.
	interimCanvas translateBy: viewedObjectBox topLeft negated 
				during: [:tempCanvas | tempCanvas drawImage: aForm at: 0@0].
	shrunkForm := interimCanvas form magnify: interimCanvas form boundingBox by: scale smoothing: 1.
	lastFormShown := shrunkForm.

	aCanvas paintImage: shrunkForm at: self center - shrunkForm boundingBox center