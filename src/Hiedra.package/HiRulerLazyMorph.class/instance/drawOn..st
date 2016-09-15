drawing
drawOn: aCanvas
	aCanvas 
		translucentImage: self sourceForm
		at: self bounds origin 
		sourceRect: self sourceRectangle