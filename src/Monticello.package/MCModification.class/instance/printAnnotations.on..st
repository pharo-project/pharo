accessing
printAnnotations: request on: aStream
	aStream nextPutAll: 'old: '.
	obsoletion printAnnotations: request on: aStream.
	aStream cr.
	aStream nextPutAll: 'new: '.
	modification printAnnotations: request on: aStream.