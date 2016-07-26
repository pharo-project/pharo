private
setClipRect: aRectOrNil
	

	aRectOrNil 
		ifNil: [ self resetClip ]
		ifNotNil: [
			self newPath;
				rectangleX: aRectOrNil left
				y: aRectOrNil top
				width: aRectOrNil width
				height: aRectOrNil height;
				primClip
			]