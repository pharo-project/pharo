printing
printOn: aStream
	aStream nextPut: $<.
	self keyword precedence = 1
		ifTrue: [ aStream nextPutAll: self keyword ]
		ifFalse: [
			self keyword keywords with: self arguments do: [ :key :arg |
				aStream nextPutAll: key; space; print: arg; space ].
			aStream skip: -1 ].
	aStream nextPut: $>.