accessing
contentType: object
	"We only allow assignment compatible with our designated mime type"
	
	| newType |
	newType := object asZnMimeType.
	contentType = newType 
		ifTrue: [ ^ self ]
		ifFalse: [ 
			(self class designatedMimeType isNil or: [ self class matches: newType ])
				ifTrue: [ contentType := newType ] ]