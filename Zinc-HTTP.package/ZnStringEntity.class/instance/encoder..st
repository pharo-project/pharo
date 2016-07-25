accessing
encoder: anEncoding
	(encoder notNil and: [ anEncoding ~= encoder ]) ifTrue: [ self invalidateContentLength ].
	encoder := anEncoding