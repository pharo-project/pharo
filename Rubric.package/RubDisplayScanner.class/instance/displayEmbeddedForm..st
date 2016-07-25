displaying
displayEmbeddedForm: aForm

	aForm 
		displayOn: bitBlt destForm 
		at: destX @ (lineY + line baseline - aForm height)
		clippingBox: bitBlt clipRect
		rule: Form blend
		fillColor: Color white