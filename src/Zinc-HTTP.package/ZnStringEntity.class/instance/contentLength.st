accessing
contentLength
	contentLength isNil ifTrue: [ self computeContentLength ].
	^ contentLength