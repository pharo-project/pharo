accessing
contentType: object
	"We only allow assignment compatible with our designated mime type"
	
	| newType |
	newType := object asZnMimeType.
	contentType = newType 
		ifTrue: [ ^ self ]
		ifFalse: [ 
			newType isBinary
				ifFalse: [ 
					contentType := newType.
					self invalidateEncoder ] ]