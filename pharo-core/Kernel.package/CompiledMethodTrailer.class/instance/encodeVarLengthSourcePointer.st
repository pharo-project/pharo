encoding
encodeVarLengthSourcePointer

	"source pointer must be >=0"
	[data >= 0] assert.
	
	encodedData := 
		data = 0 ifTrue: [ #[0] ] 
		ifFalse: [ ByteArray streamContents: [:str |
		| value |
		value := data.
		[value > 0] whileTrue: [
			value > 127 ifTrue: [ str nextPut: 128 + (value bitAnd: 16r7F) ]
				ifFalse: [ str nextPut: value. ].
			value := value >> 7.
			].
		]].
	encodedData := encodedData reversed copyWith: (self kindAsByte)