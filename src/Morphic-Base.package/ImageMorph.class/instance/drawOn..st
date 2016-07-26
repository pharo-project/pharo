drawing
drawOn: aCanvas
	"Draw the border after the image."
	
	| style |
	self isOpaque
		ifTrue: [aCanvas drawImage: image at: self innerBounds origin]
		ifFalse: [aCanvas translucentImage: image at: self innerBounds origin].
	(style := self borderStyle) ifNotNil: [style frameRectangle: bounds on: aCanvas]