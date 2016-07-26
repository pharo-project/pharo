helpers
encodeAndDecodeStream: file
	| aForm |
	file reset.
	(PNGReadWriter new on: file) understandsImageFormat ifFalse:[^self error: 'don''t understand format!' ].
	file reset.
	aForm := (PNGReadWriter new on: file) nextImage.
	aForm ifNil:[^self error: 'nil form' ].
	aForm display.
	self encodeAndDecode: aForm.
