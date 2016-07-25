instance creation
on: aCollection encoding: encodingName 
	| aTextConverter |
	encodingName 
		ifNil: [aTextConverter := TextConverter default]
		ifNotNil: [aTextConverter := TextConverter newForEncoding: encodingName].
	^ (self on: aCollection)
		converter: aTextConverter