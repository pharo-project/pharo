printing
storeOn: aStream
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' newMethod: '.
	aStream store: self size - self initialPC + 1.
	aStream nextPutAll: ' header: '.
	aStream store: self header.
	aStream nextPut: $).
	noneYet := self storeElementsFrom: self initialPC to: self endPC on: aStream.
	1 to: self numLiterals do:
		[:index |
		noneYet
			ifTrue: [noneYet := false]
			ifFalse: [aStream nextPut: $;].
		aStream nextPutAll: ' literalAt: '.
		aStream store: index.
		aStream nextPutAll: ' put: '.
		aStream store: (self literalAt: index)].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)