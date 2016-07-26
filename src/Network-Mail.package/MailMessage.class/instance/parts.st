multipart
parts
	parts ifNil: [self parseParts].
	^ parts