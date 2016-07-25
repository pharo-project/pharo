accessing
contentLength
	self hasEntity ifTrue: [ ^ self entity contentLength ].
	(self hasHeaders and: [ self headers hasContentLength ]) ifTrue: [ ^ self headers contentLength ].
	^ nil