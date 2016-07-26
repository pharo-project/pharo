accessing
contentType
	^ (self headers includesKey: 'Content-Type') 
		ifTrue: [ self headers contentType ] 
		ifFalse: [ ZnMimeType default ]