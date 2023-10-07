Extension { #name : 'Form' }

{ #category : '*FreeType-Graphics' }
Form >> ftSize [

	^ (self bitsSize * 4) + 32
]
