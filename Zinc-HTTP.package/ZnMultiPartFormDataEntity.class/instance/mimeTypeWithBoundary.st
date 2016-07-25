private
mimeTypeWithBoundary
	| mimeType |
	mimeType := self class designatedMimeType copy.
	mimeType parameterAt: 'boundary' put: self generateBoundary.
	^ mimeType