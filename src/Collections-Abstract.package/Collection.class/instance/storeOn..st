printing
storeOn: aStream 
	"Refer to the comment in Object|storeOn:."

	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet := true.
	self do: 
		[:each | 
		noneYet
			ifTrue: [noneYet := false]
			ifFalse: [aStream nextPut: $;].
		aStream nextPutAll: ' add: '.
		aStream store: each].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)