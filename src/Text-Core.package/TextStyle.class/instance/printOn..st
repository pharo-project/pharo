printing
printOn: aStream

	super printOn: aStream.
	fontArray first isFontSet ifTrue: [
		aStream space; nextPutAll: self defaultFont familySizeFace first; nextPutAll: '(FontSet)'
	] ifFalse: [
		aStream space; nextPutAll: self defaultFont familySizeFace first
	]
