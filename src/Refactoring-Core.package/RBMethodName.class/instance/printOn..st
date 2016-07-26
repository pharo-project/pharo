printing
printOn: aStream
	| argumentStream |
	self isValid
		ifFalse: [ ^ aStream nextPutAll: '(invalid)' ].
	argumentStream := self arguments readStream.
	self selector keywords keysAndValuesDo: [ :key :part |
		key = 1 ifFalse: [ aStream space ].
		aStream nextPutAll: part.
		(self selector isUnary or: [ argumentStream atEnd ]) 
			ifTrue: [ ^ self ].
		aStream space; nextPutAll: argumentStream next ]