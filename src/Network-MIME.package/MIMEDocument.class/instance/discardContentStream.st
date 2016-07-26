private
discardContentStream
	contentStream ifNotNil: [contentStream close].
	contentStream := nil