private
computeRepresentation
	representation := String streamContents: [ :stream | self writeRepresentationOn: stream ].
	contentLength := representation size