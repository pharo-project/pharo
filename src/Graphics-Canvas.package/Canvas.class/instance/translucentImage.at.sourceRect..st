drawing-images
translucentImage: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."

	(self depth < 32 or:[aForm isTranslucent not]) 
		ifTrue:[^self paintImage: aForm at: aPoint sourceRect: sourceRect].
	self image: aForm
		at: aPoint
		sourceRect: sourceRect
		rule: Form blend