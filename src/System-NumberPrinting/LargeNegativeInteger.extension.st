Extension { #name : 'LargeNegativeInteger' }

{ #category : '*System-NumberPrinting' }
LargeNegativeInteger >> printOn: aStream base: b [
	"Append a representation of this number in base b on aStream."

	aStream nextPut: $-.
	self abs printOn: aStream base: b
]
