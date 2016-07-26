printing
storeOn: aStream 
	"Use the literal form if possible."

	self shouldBePrintedAsLiteral
		ifTrue: 
			[aStream nextPut: $#; nextPut: $(.
			self do: 
				[:element | 
				element storeOn: aStream.
				aStream space].
			aStream nextPut: $)]
		ifFalse: [super storeOn: aStream]