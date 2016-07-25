drawing
drawToggleOn: aCanvas in: aRectangle

	| aForm centeringOffset |
	aForm := self toggleImageForm.
	centeringOffset := ((aRectangle height - aForm extent y) / 2.0) truncated.
	^aCanvas 
		translucentImage: aForm 
		at: (aRectangle topLeft translateBy: 0 @ centeringOffset).
