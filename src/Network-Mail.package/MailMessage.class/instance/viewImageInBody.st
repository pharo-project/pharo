printing/formatting
viewImageInBody
	| stream image |
	stream := self body contentStream.
	image := Form fromBinaryStream: stream.
	(World drawingClass withForm: image) openInWorld