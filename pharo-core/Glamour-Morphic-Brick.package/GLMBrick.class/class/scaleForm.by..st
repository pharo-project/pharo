utils
scaleForm: aForm by: aPoint

	aForm ifNil: [ ^ nil ].
	^ aForm magnify: aForm boundingBox by: aPoint smoothing: 2