private
computeRepresentation
	representation := ByteArray streamContents: [ :stream | 
		self writeRepresentationOn: (ZnBivalentWriteStream on: stream) ].
	contentLength := representation size