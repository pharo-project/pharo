instance creation
with: aCollection encoding: encodingName 
	| aTextConverter |
	encodingName 
		ifNil: [aTextConverter := TextConverter default]
		ifNotNil: [aTextConverter := TextConverter newForEncoding: encodingName].
	^ (self with: aCollection)
		converter: aTextConverter