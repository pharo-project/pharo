utils
scaleForm: aForm to: aPoint
	|scaleX scaleY|
	
	aForm ifNil: [ ^ nil ].
	scaleX := aPoint x / aForm width.
	scaleY := aPoint y / aForm height.
	^ self scaleForm: aForm by: scaleX @ scaleY.