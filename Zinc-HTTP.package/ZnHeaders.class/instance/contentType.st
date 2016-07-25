accessing
contentType
	| headerValue |
	headerValue := (self headers at: 'Content-Type') trimBoth.
	headerValue isEmpty ifTrue: [ ^ ZnMimeType default ].
	^ ZnMimeType fromString: headerValue