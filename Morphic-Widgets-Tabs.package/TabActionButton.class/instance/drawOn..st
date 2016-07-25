drawing
drawOn: aCanvas

	super drawOn: aCanvas.
	icon ifNil: [ ^ self ].
	icon form ifNil: [ ^ self ].
	aCanvas
		translucentImage: icon form
		at: self topLeft