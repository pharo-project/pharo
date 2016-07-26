drawing-images
drawImage: aForm at: aPoint sourceRect: sourceRect
	"Draw the given form."

	^self image: aForm
		at: aPoint
		sourceRect: sourceRect
		rule: Form over