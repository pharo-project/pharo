drawing
displayIconAt: aPosition on: aCanvas
	self icon ifNil: [ ^self ].
	aCanvas translucentImage: self icon at: aPosition