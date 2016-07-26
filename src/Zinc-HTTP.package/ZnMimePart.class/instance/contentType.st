accessing
contentType
	self hasEntity ifTrue: [ ^ self entity contentType ].
	(self hasHeaders and: [ self headers hasContentType ]) ifTrue: [ ^ self headers contentType ].
	^ nil